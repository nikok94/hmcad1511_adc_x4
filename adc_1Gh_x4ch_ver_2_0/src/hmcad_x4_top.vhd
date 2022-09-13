----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 28.12.2019 10:05:15
-- Design Name: 
-- Module Name: hmcad_x4_top - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_unsigned.ALL;
use ieee.std_logic_arith.all;
use IEEE.math_real.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
library UNISIM;
use UNISIM.VComponents.all;

library work;
use work.clock_generator;
use work.spi_adc_250x4_master;
use work.hmcad_x4_block;
use work.QSPI_interconnect;
use work.trigger_capture;
use work.data_recorder;
--use work.defPulse;


entity hmcad_x4_top is
    Port (
        in_clk_50MHz            : in std_logic;
        in_clk_20MHz            : in std_logic;
        xc_sys_rstn             : in std_logic;
    
        adc0_lclk_p             : in std_logic;
        adc0_lclk_n             : in std_logic;
        adc0_fclk_p             : in std_logic;
        adc0_fclk_n             : in std_logic;
        adc0_dx_a_p             : in std_logic_vector(3 downto 0);
        adc0_dx_a_n             : in std_logic_vector(3 downto 0);
        adc0_dx_b_p             : in std_logic_vector(3 downto 0);
        adc0_dx_b_n             : in std_logic_vector(3 downto 0);

        adc1_lclk_p             : in std_logic;
        adc1_lclk_n             : in std_logic;
        adc1_fclk_p             : in std_logic;
        adc1_fclk_n             : in std_logic;
        adc1_dx_a_p             : in std_logic_vector(3 downto 0);
        adc1_dx_a_n             : in std_logic_vector(3 downto 0);
        adc1_dx_b_p             : in std_logic_vector(3 downto 0);
        adc1_dx_b_n             : in std_logic_vector(3 downto 0);

        adc2_lclk_p             : in std_logic;
        adc2_lclk_n             : in std_logic;
        adc2_fclk_p             : in std_logic;
        adc2_fclk_n             : in std_logic;
        adc2_dx_a_p             : in std_logic_vector(3 downto 0);
        adc2_dx_a_n             : in std_logic_vector(3 downto 0);
        adc2_dx_b_p             : in std_logic_vector(3 downto 0);
        adc2_dx_b_n             : in std_logic_vector(3 downto 0);

        adc3_lclk_p             : in std_logic;
        adc3_lclk_n             : in std_logic;
        adc3_fclk_p             : in std_logic;
        adc3_fclk_n             : in std_logic;
        adc3_dx_a_p             : in std_logic_vector(3 downto 0);
        adc3_dx_a_n             : in std_logic_vector(3 downto 0);
        adc3_dx_b_p             : in std_logic_vector(3 downto 0);
        adc3_dx_b_n             : in std_logic_vector(3 downto 0);

        fpga_sck                : in std_logic;
        fpga_cs                 : in std_logic;
        fpga_miso               : out std_logic;
        fpga_mosi               : in std_logic;
        
        spifi_cs                : in std_logic;
        spifi_sck               : in std_logic;
        spifi_sio               : inout std_logic_vector(3 downto 0);
        
        int_adcx                : out std_logic_vector(3 downto 0);
        
        i2c_sda                 : in std_logic;
        i2c_scl                 : in std_logic;
        
        dd                      : inout std_logic_vector(7 downto 0);
        clk_dd                  : in std_logic;
        cs_dd                   : in std_logic;
        
        pulse_n                 : out std_logic := '0';
        pulse_p                 : out std_logic := '0';
        
        led                     : out std_logic;

        vic_pulse               : out std_logic

        );
end hmcad_x4_top;

architecture Behavioral of hmcad_x4_top is
    constant C_BURST_WIDTH_SPIFI        : integer := 16;
    constant c_max_num_data             : integer := 256;  --2048;
    constant c_channel_num              : integer := 4;
    signal wr_rec_cnt_max               : std_logic_vector(32 - 1 downto 0):= (others => '0');
    signal wr_rec_cnt                   : std_logic_vector(32 - 1 downto 0);
    signal sys_rst                      : std_logic;
    signal pll_lock                     : std_logic;
    signal clk_137_5MHz                 : std_logic;
    signal clk_250MHz                   : std_logic;
    signal rst                          : std_logic;
    signal infrst_rst_out               : std_logic;
    
    type SPIRegistersStrucrure       is (TriggerSetUp, ADCEnableReg, TriggerPositionSetUp, ControlReg, BufferLength, PulseOffset, MarkOffset, MarkLength, StructureLength);
    type SPIRegistersType    is array (SPIRegistersStrucrure'pos(StructureLength) - 1 downto 0) of std_logic_vector(15 downto 0);
    signal SPIRegisters                 : SPIRegistersType := (
                                            SPIRegistersStrucrure'pos(TriggerSetUp) => x"7F00",
                                            SPIRegistersStrucrure'pos(ADCEnableReg) => x"0007",
                                            SPIRegistersStrucrure'pos(TriggerPositionSetUp) => x"0800",
                                            SPIRegistersStrucrure'pos(ControlReg) => x"0000",
                                            SPIRegistersStrucrure'pos(BufferLength) => conv_std_logic_vector(c_max_num_data, 16),
                                            SPIRegistersStrucrure'pos(PulseOffset) => x"0000",
                                            SPIRegistersStrucrure'pos(MarkOffset) => x"0014",
                                            SPIRegistersStrucrure'pos(MarkLength) => x"0004",
                                            others => (others => '0')
                                            );

    type ControlRegType             is (program_rst,
                                        mode_0,
                                        mode_1,
                                        pulse_start,
                                        buffer_rst,
                                        test_vic_start,
                                        StructureLength);
    signal MISO_I                       : std_logic := '0';
    signal MISO_O                       : std_logic;
    signal MISO_T                       : std_logic;
    signal MOSI_I                       : std_logic;
    signal MOSI_O                       : std_logic;
    signal MOSI_T                       : std_logic;
    signal m_fcb_aresetn                : std_logic;
    signal m_fcb_addr                   : std_logic_vector(8 - 1 downto 0);
    signal m_fcb_wrdata                 : std_logic_vector(16 - 1 downto 0);
    signal m_fcb_wrreq                  : std_logic;
    signal m_fcb_wrreq_d                : std_logic;
    signal m_fcb_wrack                  : std_logic;
    signal m_fcb_rddata                 : std_logic_vector(16 - 1 downto 0);
    signal m_fcb_rdreq                  : std_logic;
    signal m_fcb_rdack                  : std_logic;
    
    signal reg_address_int              : integer;
    
    signal adcx_calib_done              : std_logic_vector(3 downto 0);

    signal acfg_bits                    : std_logic_vector(15 downto 0);
    signal aext_trig                    : std_logic;
    signal trig_start                   : std_logic;
    signal trig_position                : std_logic_vector(15 downto 0);
    signal spi_rst_cmd                  : std_logic:= '1';
--    signal hmcad_x4_block_rst           : std_logic;
    signal trigger_mode                 : std_logic_vector(1 downto 0);
    signal trigger_start                : std_logic;
--    signal trigger_start_counter        : integer;
--    signal trigger_start_delay          : std_logic_vector(3 downto 0);
    
    signal state_out                    : integer;
--    signal pulse                        : std_logic;
--    signal pulse_cnt                    : std_logic_vector(7 downto 0);
    signal start_pulse                  : std_logic:='0';
    signal pulse_out                    : std_logic:='0';
    signal pulse_out_d                  : std_logic:='0';
    signal pulse_out_res                : std_logic:='0';
    
    
    constant calid_done_delay           : integer := 10000000;
    signal trigger_start_out            : std_logic;
    signal spifi_sck_bufg               : std_logic;
    
    signal rec_x_en                     : std_logic_vector(c_channel_num - 1 downto 0);
    signal rec_x_valid                  : std_logic_vector(c_channel_num - 1 downto 0);
    signal rec_x_ready                  : std_logic_vector(c_channel_num - 1 downto 0);
    signal rec_x_data                   : std_logic_vector(c_channel_num * 64 - 1 downto 0);
    
    signal hmcad_x_int                  : std_logic_vector(c_channel_num - 1 downto 0);
    signal hmcad_x_clk                  : std_logic_vector(c_channel_num - 1 downto 0);
    signal hmcad_x_valid                : std_logic_vector(c_channel_num - 1 downto 0);
    signal hmcad_x_valid_array          : std_logic_vector(c_channel_num - 1 downto 0);
    type hmcad_x_data_type              is array (c_channel_num - 1 downto 0) of std_logic_vector(63 downto 0);
    signal hmcad_x_data_array           : std_logic_vector(c_channel_num * 64 - 1 downto 0);
    signal hmcad_x_data                 : hmcad_x_data_type;
    type hmcad_x_cnt_type               is array (c_channel_num - 1 downto 0) of std_logic_vector(31 downto 0);
    signal hmcad_x_cnt                  : hmcad_x_cnt_type;

    signal hmcad_x_cnt_test_0           : std_logic_vector(c_channel_num * 32 - 1 downto 0);
    signal hmcad_x_cnt_test_1           : std_logic_vector(c_channel_num * 32 - 1 downto 0);
    signal hmcad_x_cnt_test_2           : std_logic_vector(c_channel_num * 32 - 1 downto 0);
    
    signal qspi_x_cs_up                 : std_logic_vector(c_channel_num - 1 downto 0);
    signal qspi_x_ready                 : std_logic_vector(c_channel_num - 1 downto 0);
    signal qspi_x_en                    : std_logic_vector(c_channel_num - 1 downto 0);
    signal qspi_x_data                  : std_logic_vector(c_channel_num * 64 - 1 downto 0);
    
    signal adcx_lclk_p                  : std_logic_vector(c_channel_num - 1 downto 0);
    signal adcx_lclk_n                  : std_logic_vector(c_channel_num - 1 downto 0);
    signal adcx_fclk_p                  : std_logic_vector(c_channel_num - 1 downto 0);
    signal adcx_fclk_n                  : std_logic_vector(c_channel_num - 1 downto 0);
    signal adcx_dx_a_p                  : std_logic_vector(c_channel_num * 4 - 1 downto 0);
    signal adcx_dx_a_n                  : std_logic_vector(c_channel_num * 4 - 1 downto 0);
    signal adcx_dx_b_p                  : std_logic_vector(c_channel_num * 4 - 1 downto 0);
    signal adcx_dx_b_n                  : std_logic_vector(c_channel_num * 4 - 1 downto 0);
    
    
    --signal slave_x_clk                  : std_logic_vector(3 - 1 downto 0);
    --signal slave_x_ready                : std_logic_vector(3 - 1 downto 0);
    --signal slave_x_data                 : std_logic_vector(3*64 - 1 downto 0);
    --signal slave_x_cs_up                : std_logic_vector(3 - 1 downto 0);
    --
    --signal dds                           : std_logic_vector(7 downto 0);
    
    signal clk_cnt                      : integer;
    signal led_s                        : std_logic;
    signal adcxTrigger                  : std_logic;
    signal adcxSyncPulse                : std_logic;
--    signal adcxTriggerDef_p             : std_logic_vector(3 downto 0);
--    signal adcxTriggerDef_n             : std_logic_vector(3 downto 0);
--    signal adcxTriggerRes_p             : std_logic_vector(2 downto 0);
--    signal adcxTriggerRes_n             : std_logic_vector(2 downto 0);
    signal repeat_vec                   : std_logic_vector(2 downto 0);
    signal repeat_start                 : std_logic:='0';
--    signal repeat_rst                   : std_logic:='0';
    signal rep_state                    : std_logic_vector(7 downto 0);
    signal rep_cnt                      : std_logic_vector(7 downto 0);
    signal hmcad_x4_block_start         : std_logic;
    signal hmcad_x4_active_status       : std_logic_vector(3 downto 0);
    signal hmcad_x4_active_statusClear  : std_logic;
    signal hmcad_x4_activeWD            : std_logic_vector(7 downto 0);
    signal hmcad_x4_activeCnt           : std_logic_vector(7 downto 0);
    signal hmcad_x4_calibDone           : std_logic;
    signal hmcad_x4_adc_enable          : std_logic_vector(c_channel_num - 1 downto 0);
    signal hmcad_buffer_rst             : std_logic;

    signal trigger_valid_in             : std_logic;
    signal trigger_cnt_in               : std_logic_vector(31 downto 0);
    signal trigger_cnt_out              : std_logic_vector(31 downto 0);
    signal trigger_out                  : std_logic;
    signal trigger_data_in              : std_logic_vector(63 downto 0);
    signal trigger_rst                  : std_logic;

    signal rec_stop                     : std_logic;
    signal rec_rst                      : std_logic;
    signal rec_stop_cnt                 : std_logic_vector(32 - 1 downto 0);

    signal rec_cnt_ofst                 : std_logic_vector(32 - 1 downto 0);
    signal rec_cnt_int                  : integer;
    signal rec_rdy                      : std_logic_vector(c_channel_num - 1 downto 0);

    signal mux_data_selector            : std_logic_vector(1 downto 0);
    type rec_state_type                 is (IDLE, FIFO_RST, WAIT_ADC_CALIB_DONE, RST_LOGIC, SYNC_PULSE_STATE, WAIT_REC_RDY, WAIT_TRG, NORMAL_STRT, AUTO_STRT, EXT_STRT, REC_STOP_CMD, WAIT_REC_IRQ, WAIT_QSPI);
    signal rec_state                    : rec_state_type;
    signal rec_state_rst                : std_logic;
    signal areset_fifo                  : std_logic;
    
begin
wr_rec_cnt_max(natural(round(log2(real(c_max_num_data)))) - 1 downto 0) <= (others => '1');
rst <= infrst_rst_out;

defPulse0_inst_gen : for i in 0 to hmcad_x_clk'length-1 generate
  process(hmcad_x_clk(i), hmcad_x4_active_statusClear)
  begin
    if (hmcad_x4_active_statusClear = '1') then
      hmcad_x4_active_status(i) <= '0';
    elsif rising_edge(hmcad_x_clk(i)) then
      hmcad_x4_active_status(i) <= '1';
    end if;
  end process;
end generate;

process(clk_137_5MHz, rst)
begin
  if (rst = '1') then
    hmcad_x4_activeWD <= (others => '0');
    hmcad_x4_calibDone <= '0';
  elsif rising_edge(clk_137_5MHz) then
    case (hmcad_x4_activeWD) is
      when x"00" =>
        hmcad_x4_active_statusClear <= '1';
        hmcad_x4_activeWD <= x"01";
        hmcad_x4_activeCnt <= (others => '0');
      when x"01" =>
        hmcad_x4_active_statusClear <= '1';
        if (hmcad_x4_activeCnt < 3) then
          hmcad_x4_activeCnt <= hmcad_x4_activeCnt + 1;
        else
          hmcad_x4_activeCnt <= (others => '0');
          hmcad_x4_activeWD <= x"02";
        end if;
      when x"02" =>
        hmcad_x4_active_statusClear <= '0';
        if (hmcad_x4_activeCnt < 3) then
          hmcad_x4_activeCnt <= hmcad_x4_activeCnt + 1;
        else
          hmcad_x4_activeCnt <= (others => '0');
          hmcad_x4_activeWD <= x"03";
        end if;
      when x"03" =>
        if ((hmcad_x4_active_status and hmcad_x4_adc_enable) = hmcad_x4_adc_enable) then
          hmcad_x4_activeWD <= x"04";
        else
          hmcad_x4_activeWD <= x"00";
          hmcad_x4_calibDone <= '0';
        end if;
      when x"04" =>
        if ((adcx_calib_done and hmcad_x4_adc_enable) = hmcad_x4_adc_enable) then
          hmcad_x4_calibDone <= '1';
        else
          hmcad_x4_calibDone <= '0';
        end if;
        hmcad_x4_activeWD <= x"00";
      when others =>
        hmcad_x4_activeWD <= (others => '0');
    end case;
  end if;
end process;

hmcad_x4_adc_enable <= SPIRegisters(SPIRegistersStrucrure'pos(ADCEnableReg))(hmcad_x4_active_status'length - 1 downto 0);

dd(dd'length - 1 downto 8) <= (others => 'Z');
dd(6) <= adcx_calib_done(3);
dd(5) <= adcx_calib_done(2);
dd(4) <= adcx_calib_done(1);
dd(3) <= adcx_calib_done(0);
dd(1) <= hmcad_x4_calibDone;--(adcx_calib_done(3) and adcx_calib_done(2)) and (adcx_calib_done(1) and adcx_calib_done(0));
dd(0) <= pll_lock;

rep_state_proc :
process(clk_137_5MHz, rst)
begin
  if (rst = '1') then
    repeat_vec <= (others => '0');
    rep_state <= (others => '0');
    repeat_start <= '0';
  elsif rising_edge(clk_137_5MHz) then
    repeat_vec(0) <= dd(2);
    repeat_vec(repeat_vec'length - 1 downto 1) <= repeat_vec(repeat_vec'length - 2 downto 0);
    case (rep_state) is
      when x"00" =>
        if (repeat_vec(repeat_vec'length - 1) = '0' and repeat_vec(repeat_vec'length - 2)='1') then
          rep_cnt <= (others => '0');
          rep_state <= x"01";
        end if;
        repeat_start <= '0';
      when x"01" =>
        if (repeat_vec(repeat_vec'length - 2) = '1') then
          repeat_start <= '1';
          rep_state <= x"00";
        else
          repeat_start <= '0';
          rep_state <= x"00";
        end if;
      when others =>
        rep_state <= x"00";
    end case;
  end if;
end process;

sys_rst <= (not xc_sys_rstn);

Clock_gen_inst : entity clock_generator
    Port map( 
      clk_in            => in_clk_50MHz,
      rst_in            => sys_rst,
      pll_lock          => pll_lock,
      clk_out           => clk_137_5MHz,
--      clk_out_250MHz    => clk_250MHz,
      rst_out           => infrst_rst_out
    );

spi_fcb_master_inst : entity spi_adc_250x4_master
    generic map(
      C_CPHA            => 1,
      C_CPOL            => 1,
      C_LSB_FIRST       => 0
    )
    Port map( 
      SCK               => fpga_sck,
      CS                => fpga_cs,

      MISO_I            => MISO_I,
      MISO_O            => MISO_O,
      MISO_T            => MISO_T,
      MOSI_I            => MOSI_I,
      MOSI_O            => MOSI_O,
      MOSI_T            => MOSI_T,

      m_fcb_clk         => clk_137_5MHz,
      m_fcb_areset      => infrst_rst_out,
      m_fcb_addr        => m_fcb_addr,
      m_fcb_wrdata      => m_fcb_wrdata,
      m_fcb_wrreq       => m_fcb_wrreq,
      m_fcb_wrack       => m_fcb_wrack,
      m_fcb_rddata      => m_fcb_rddata,
      m_fcb_rdreq       => m_fcb_rdreq,
      m_fcb_rdack       => m_fcb_rdack
    );

OBUFT_inst : OBUFT
   generic map (
      DRIVE => 12,
      IOSTANDARD => "DEFAULT",
      SLEW => "SLOW")
   port map (
      O => fpga_miso,     -- Buffer output (connect directly to top-level port)
      I => MISO_O,     -- Buffer input
      T => MISO_T      -- 3-state enable input 
   );

MOSI_I <= fpga_mosi;

-------------------------------------------------
-- управляющие регистры 
-------------------------------------------------
-- процесс записи/чтения регистров управления
-------------------------------------------------
spi_write_process :
  process(rst, clk_137_5MHz)
  begin
    if (rst = '1') then
      m_fcb_wrack <= '0';
      m_fcb_rdack <= '0';
      trigger_mode <= (others => '0');
      SPIRegisters(SPIRegistersStrucrure'pos(ControlReg))(ControlRegType'pos(StructureLength) - 1 downto 0) <= (others => '0');
    elsif rising_edge(clk_137_5MHz) then
       m_fcb_wrreq_d <= m_fcb_wrreq;
      if ((m_fcb_wrreq = '1') and (m_fcb_wrreq_d = '0'))then
        if ((conv_integer(m_fcb_addr) = SPIRegistersStrucrure'pos(ControlReg)) and m_fcb_wrdata(ControlRegType'pos(mode_1) downto ControlRegType'pos(mode_0)) /= "00") then
          trigger_mode <= m_fcb_wrdata(ControlRegType'pos(mode_1) downto ControlRegType'pos(mode_0));
        end if;
        m_fcb_wrack <= '1';
        SPIRegisters(conv_integer(m_fcb_addr)) <= m_fcb_wrdata;
      elsif (m_fcb_rdreq = '1') then
        m_fcb_rdack <= '1';
        m_fcb_rddata <= SPIRegisters(conv_integer(m_fcb_addr));
      else
        m_fcb_wrack <= '0';
        m_fcb_rdack <= '0';
        SPIRegisters(SPIRegistersStrucrure'pos(ControlReg))(ControlRegType'pos(StructureLength) - 1 downto 0) <= (others => '0');
      end if;
      SPIRegisters(SPIRegistersStrucrure'pos(BufferLength)) <= conv_std_logic_vector(c_max_num_data, 16);
    end if;
  end process;

vic_pulse <= SPIRegisters(SPIRegistersStrucrure'pos(ControlReg))(ControlRegType'pos(test_vic_start));
start_pulse <= SPIRegisters(SPIRegistersStrucrure'pos(ControlReg))(ControlRegType'pos(pulse_start));
hmcad_buffer_rst <= SPIRegisters(SPIRegistersStrucrure'pos(ControlReg))(ControlRegType'pos(buffer_rst));
trigger_start <= SPIRegisters(SPIRegistersStrucrure'pos(ControlReg))(ControlRegType'pos(mode_1)) or SPIRegisters(SPIRegistersStrucrure'pos(ControlReg))(ControlRegType'pos(mode_0));
spi_rst_cmd <= SPIRegisters(SPIRegistersStrucrure'pos(ControlReg))(ControlRegType'pos(program_rst));

pulse_proc :
  process(clk_137_5MHz, rst)
  begin
    if (rst = '1') then
      pulse_out <= '0';
    elsif rising_edge(clk_137_5MHz) then
      pulse_out <= start_pulse or adcxSyncPulse;
      pulse_out_d <= pulse_out;
      pulse_out_res <= (not pulse_out_d) and pulse_out;
    end if;
  end process;


pulse_out_proc : process(clk_137_5MHz)
begin
  if rising_edge(clk_137_5MHz) then
    if (SPIRegisters(SPIRegistersStrucrure'pos(TriggerSetUp))(6) = '1') then
      pulse_p <= pulse_out_res;-- or adcxTriggerRes(2) ;
      pulse_n <= not pulse_out_res;--adcxTriggerRes_n(2);--not (pulse_out or adcxTriggerRes(2));
    else
      pulse_n <= pulse_out_res;-- or adcxTriggerRes(2) ;
      pulse_p <= not pulse_out_res;--adcxTriggerRes_n(2);--not (pulse_out or adcxTriggerRes(2));
    end if;
  end if;
end process;

spifi_sck_bufg <= spifi_sck;

adcx_lclk_p <= adc3_lclk_p & adc2_lclk_p & adc1_lclk_p & adc0_lclk_p;
adcx_lclk_n <= adc3_lclk_n & adc2_lclk_n & adc1_lclk_n & adc0_lclk_n;
adcx_fclk_p <= adc3_fclk_p & adc2_fclk_p & adc1_fclk_p & adc0_fclk_p;
adcx_fclk_n <= adc3_fclk_n & adc2_fclk_n & adc1_fclk_n & adc0_fclk_n;
adcx_dx_a_p <= adc3_dx_a_p & adc2_dx_a_p & adc1_dx_a_p & adc0_dx_a_p;
adcx_dx_a_n <= adc3_dx_a_n & adc2_dx_a_n & adc1_dx_a_n & adc0_dx_a_n;
adcx_dx_b_p <= adc3_dx_b_p & adc2_dx_b_p & adc1_dx_b_p & adc0_dx_b_p;
adcx_dx_b_n <= adc3_dx_b_n & adc2_dx_b_n & adc1_dx_b_n & adc0_dx_b_n;


hmcad_x4_block_start <= repeat_start or trigger_start;

rec_state_rst <= rst or spi_rst_cmd;

rec_state_process : process(clk_137_5MHz, rec_state_rst)
begin
  if (rec_state_rst = '1') then
    rec_state <= IDLE;
    rec_rst <= '1';
    adcxSyncPulse <= '0';
    trigger_rst <= '1';
    rec_stop <= '0';
    wr_rec_cnt <= wr_rec_cnt_max;
    rec_stop_cnt <= (others => '0');
  elsif rising_edge(clk_137_5MHz) then
    case rec_state is
      when IDLE =>
        if (hmcad_x4_block_start = '1') then
          rec_state <= WAIT_ADC_CALIB_DONE;
          wr_rec_cnt <= wr_rec_cnt_max;
        end if;
        rec_rst <= '0';
        trigger_rst <= '1';
        rec_stop <= '0';
        adcxSyncPulse <= '0';
      when WAIT_ADC_CALIB_DONE =>
        if (hmcad_x4_calibDone = '1') then
          rec_rst <= '1';
          rec_state <= RST_LOGIC;
        end if;
      when RST_LOGIC =>
        rec_state <= SYNC_PULSE_STATE;
        rec_rst <= '1';
      when SYNC_PULSE_STATE => 
        rec_state <= WAIT_REC_RDY;
        adcxSyncPulse <= '1';
        rec_rst <= '0';
      when WAIT_REC_RDY =>
        adcxSyncPulse <= '0'; 
        if (rec_rdy = hmcad_x4_adc_enable) then
          rec_state <= WAIT_TRG;
          wr_rec_cnt <= wr_rec_cnt - (x"0000" & SPIRegisters(SPIRegistersStrucrure'pos(MarkLength)));
        end if;
      when WAIT_TRG =>
        if (trigger_mode = "01") then-- normal start
          rec_state <= NORMAL_STRT;
          trigger_rst <= '0';
          wr_rec_cnt <= wr_rec_cnt - (x"0000" & SPIRegisters(SPIRegistersStrucrure'pos(TriggerPositionSetUp)));
        elsif (trigger_mode = "11") then -- ext start
          rec_state <= EXT_STRT;
        else --auto start
          rec_state <= AUTO_STRT;
        end if;
      when NORMAL_STRT => -- normal start
        if (trigger_out = '1') then
          rec_state <= REC_STOP_CMD; 
        end if;
      when EXT_STRT => -- ext start
          rec_state <= REC_STOP_CMD; 
      when AUTO_STRT => -- auto start
          rec_state <= REC_STOP_CMD;
      when REC_STOP_CMD =>
        rec_stop_cnt <= trigger_cnt_out + wr_rec_cnt;
        trigger_rst <= '1';
        rec_stop <= '1';
        rec_state <= WAIT_REC_IRQ;
      when WAIT_REC_IRQ =>
        rec_stop <= '0';
        if (hmcad_x_int = hmcad_x4_adc_enable) then
          rec_state <= WAIT_QSPI;
        end if;
      when WAIT_QSPI =>
        if (hmcad_x_int = "0000") then
          rec_state <= IDLE;
        end if;
      when others => 
        rec_state <= IDLE;
    end case;
  end if;
end process;



hmcad_x4_block_inst : entity hmcad_x4_block
  generic map(
    c_channel_number        => c_channel_num
  )
  port map(
    clk                     => clk_137_5MHz,
    areset_fifo             => areset_fifo,
    areset                  => rst, 
    enable                  => hmcad_x4_adc_enable,
    calib_done              => adcx_calib_done,

    adcx_lclk_p             => adcx_lclk_p,
    adcx_lclk_n             => adcx_lclk_n,
    adcx_fclk_p             => adcx_fclk_p,
    adcx_fclk_n             => adcx_fclk_n,
    adcx_dx_a_p             => adcx_dx_a_p,
    adcx_dx_a_n             => adcx_dx_a_n,
    adcx_dx_b_p             => adcx_dx_b_p,
    adcx_dx_b_n             => adcx_dx_b_n,

    adcx_clk_out            => hmcad_x_clk,
--    cnt_rst                 => cnt_rst,
--    cnt_out                 => hmcad_x_cnt,
    data_out                => hmcad_x_data_array,
    valid_out               => hmcad_x_valid_array
    );

areset_fifo <= hmcad_buffer_rst or rec_rst;


adc_channel_gen : for i in 0 to c_channel_num - 1 generate

  hmcad_x_cnt_proc : process(clk_137_5MHz, areset_fifo)
  begin
    if (areset_fifo = '1') then
      hmcad_x_cnt(i) <= (others => '0');
      hmcad_x_data(i) <= (others => '0');
    elsif rising_edge(clk_137_5MHz) then
      if (hmcad_x_valid_array(i) = '1') then
        hmcad_x_cnt(i) <= hmcad_x_cnt(i) + 1;
        hmcad_x_data(i) <= hmcad_x_data_array(i * 64 + 63 downto i * 64);
      end if;
      hmcad_x_valid(i) <= hmcad_x_valid_array(i);
    end if;
  end process;
  
   data_recorder_inst : entity data_recorder
   generic map(
     c_max_num_data            => c_max_num_data,
     c_data_width              => 64,
     c_cnt_width               => 32
   )
   port map( 
     rst                       => areset_fifo,

     mark_delay                => SPIRegisters(SPIRegistersStrucrure'pos(MarkOffset))(natural(round(log2(real(c_max_num_data)))) - 1 downto 0),
     mark_cnt                  => SPIRegisters(SPIRegistersStrucrure'pos(MarkLength))(natural(round(log2(real(c_max_num_data)))) - 1 downto 0),

     stop                      => rec_stop,
     stop_cnt                  => rec_stop_cnt,

     s_clk                     => clk_137_5MHz,
     s_cnt                     => hmcad_x_cnt(i),
     s_data                    => hmcad_x_data(i),
     s_valid                   => hmcad_x_valid(i),
     s_ready                   => rec_rdy(i),

     m_clk                     => clk_137_5MHz,
     m_en                      => rec_x_en(i),
     m_data                    => rec_x_data(i*64 + 63 downto i*64),
     m_valid                   => rec_x_valid(i),
     m_ready                   => rec_x_ready(i)
   );

  process(clk_137_5MHz, qspi_x_cs_up(i), areset_fifo )
  begin
    if ((qspi_x_cs_up(i) = '1') or (areset_fifo = '1')) then
      hmcad_x_int(i) <= '0';
    elsif rising_edge(clk_137_5MHz) then
      if (rec_x_valid(i) = '1') then
        hmcad_x_int(i) <= '1';
      end if;
    end if;
  end process;
  
end generate;

mux_data_selector <= SPIRegisters(SPIRegistersStrucrure'pos(TriggerSetUp))(3 downto 2);

--process(hmcad_x_data, hmcad_x_cnt, hmcad_x_valid, mux_data_selector)
--begin
--  case mux_data_selector is
--    when "00" => 
--      trigger_data_in <= hmcad_x_data(0);
--      trigger_cnt_in <= hmcad_x_cnt(0);
--      trigger_valid_in <= hmcad_x_valid(0);
--    when "01" => 
--      trigger_data_in <= hmcad_x_data(1);
--      trigger_cnt_in <= hmcad_x_cnt(1);
--      trigger_valid_in <= hmcad_x_valid(1);
--    when "10" => 
--      trigger_data_in <= hmcad_x_data(2);
--      trigger_cnt_in <= hmcad_x_cnt(2);
--      trigger_valid_in <= hmcad_x_valid(2);
--    when others =>
--      trigger_data_in <= hmcad_x_data(3);
--      trigger_cnt_in <= hmcad_x_cnt(3);
--      trigger_valid_in <= hmcad_x_valid(3);
--  end case;
--end process;

p_parity_check : process (clk_137_5MHz)
variable vparity_data           : std_logic_vector(63 downto 0);
variable vparity_cnt            : std_logic_vector(31 downto 0);
variable vparity_valid          : std_logic;
begin
  if rising_edge(clk_137_5MHz) then
    vparity_data := (others => '0');
    vparity_cnt := (others => '0');
    vparity_valid := '0';
    l_parity : for k in 0 to c_channel_num - 1 loop
      if (k = conv_integer(unsigned(mux_data_selector))) then
        vparity_data := hmcad_x_data(k);
        vparity_cnt := hmcad_x_cnt(k);
        vparity_valid := hmcad_x_valid(k);
      end if;
    end loop l_parity;
    
    trigger_data_in <= vparity_data;
    trigger_cnt_in <= vparity_cnt;
    trigger_valid_in <= vparity_valid;
  end if;
end process p_parity_check;


trigger_capture_inst: entity trigger_capture
    generic map(
      c_data_width      => 64,
      c_cnt_width       => 32
    )
    Port map(
      clk               => clk_137_5MHz,
      rst               => trigger_rst,

      front             => SPIRegisters(SPIRegistersStrucrure'pos(TriggerSetUp))(5 downto 4),
      level             => SPIRegisters(SPIRegistersStrucrure'pos(TriggerSetUp))(15 downto 8),

      data              => trigger_data_in,
      cnt               => trigger_cnt_in,
      valid             => trigger_valid_in,
      trigger           => trigger_out,
      cnt_out           => trigger_cnt_out
    );

int_adcx <= hmcad_x_int;
rec_x_ready <= qspi_x_ready;
qspi_x_data <= rec_x_data;
rec_x_en <= qspi_x_en;
 
QSPI_interconnect_inst : entity QSPI_interconnect
  Generic map(
    c_num_slave_port    => 4,
    c_data_width        => 64,
    c_command_width     => 8,
    C_CPHA              => '0',
    C_CPOL              => '0',
    C_LSB_FIRST         => false
  )
  Port map(
    slave_x_clk         => clk_137_5MHz,
    slave_x_en          => qspi_x_en,
    slave_x_ready       => qspi_x_ready,
    slave_x_data        => qspi_x_data ,
    slave_x_cs_up       => qspi_x_cs_up,
    qspi_sio            => spifi_sio,
    qspi_sck            => spifi_sck_bufg,
    qspi_cs             => spifi_cs
  );
  
led_process : process(infrst_rst_out, clk_137_5MHz)
begin
  if (infrst_rst_out = '1') then
    led_s <= '0';
    clk_cnt <= 0;
  elsif rising_edge(clk_137_5MHz) then
    if (clk_cnt < 137500000 / 2 - 1) then
      clk_cnt <= clk_cnt + 1;
    else
      clk_cnt <= 0;
      led_s <= not led_s; 
    end if;
  end if;
end process;

led <= led_s;

end Behavioral;
