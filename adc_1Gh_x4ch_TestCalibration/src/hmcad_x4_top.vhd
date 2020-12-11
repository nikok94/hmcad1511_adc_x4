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
use work.aFifo;
use work.phaseAnalyzer_basedDCM;


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
        
        controls_out            : out std_logic_vector(4 downto 0);
        
        pulse_n                 : out std_logic := '0';
        pulse_p                 : out std_logic := '0'

        );
end hmcad_x4_top;

architecture Behavioral of hmcad_x4_top is
    constant C_BURST_WIDTH_SPIFI        : integer := 16;
    constant c_max_num_data             : integer := 128;  --2048;
    constant qspi_num_slave_port        : integer := 4;
    constant sync_stage                 : integer := 3;
    signal sys_rst                      : std_logic;
    signal pll_lock                     : std_logic;
    signal clk_125MHz                   : std_logic;
    signal clk_250MHz                   : std_logic;
    signal rst                          : std_logic;
    signal infrst_rst_out               : std_logic;
    signal adcx_fclk_out                : std_logic_vector(3 downto 0);
    
    type SPIRegistersStrucrure       is (TriggerSetUp, ADCEnableReg, TriggerPositionSetUp, ControlReg, BufferLength, HMCADCounter, StructureLength);
    type SPIRegistersType    is array (SPIRegistersStrucrure'pos(StructureLength) + 16 - 1 downto 0) of std_logic_vector(15 downto 0);
    signal SPIRegisters                 : SPIRegistersType := (
                                            SPIRegistersStrucrure'pos(TriggerSetUp) => x"7F00",
                                            SPIRegistersStrucrure'pos(ADCEnableReg) => x"0007",
                                            SPIRegistersStrucrure'pos(TriggerPositionSetUp) => x"0800",
                                            SPIRegistersStrucrure'pos(ControlReg) => x"0000",
                                            others => (others => '0')
                                            );

    type ControlRegType             is (program_rst,
                                        mode_0,
                                        mode_1,
                                        pulse_start,
                                        buffer_rst,
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
    signal m_fcb_wrack                  : std_logic;
    signal m_fcb_rddata                 : std_logic_vector(16 - 1 downto 0);
    signal m_fcb_rdreq                  : std_logic;
    signal m_fcb_rdack                  : std_logic;
    
    signal reg_address_int              : integer;
    
    signal adcx_all_calib_done          : std_logic;
    signal adcx_calib_done              : std_logic_vector(3 downto 0);
    signal adcx_calib_done_d            : std_logic_vector(3 downto 0);
    signal adcx_tick_ms                 : std_logic_vector(3 downto 0);
    signal adcx_tick_ms_d0              : std_logic_vector(3 downto 0);
    signal adcx_tick_ms_d1              : std_logic_vector(3 downto 0);
    
    signal adcx_tick_ms_counter0        : integer := 0;
    signal adcx_tick_ms_counter1        : integer := 0;
    signal adcx_tick_ms_counter2        : integer := 0;
    signal adcx_tick_ms_counter3        : integer := 0;

    
    signal acfg_bits                    : std_logic_vector(15 downto 0);
    signal aext_trig                    : std_logic;
    signal trig_start                   : std_logic;
    signal trig_position                : std_logic_vector(15 downto 0);
    signal spi_rst_cmd                  : std_logic;
    signal hmcad_x4_block_rst           : std_logic;
    signal trigger_mode                 : std_logic_vector(1 downto 0);
    signal trigger_start                : std_logic;
    signal trigger_start_counter        : integer;
    signal trigger_start_delay          : std_logic_vector(3 downto 0);
    
    signal state_out                    : integer;
    signal pulse                        : std_logic;
    signal pulse_cnt                    : std_logic_vector(2 downto 0);
    signal start_pulse                  : std_logic;
    signal pulse_out                    : std_logic;
    
    signal pulse_sync_vec               : std_logic_vector(sync_stage-1 downto 0);
    signal pulse_sync                   : std_logic;
    
    constant calid_done_delay           : integer := 10000000;
    signal trigger_start_out            : std_logic;
    signal spifi_sck_bufg               : std_logic;

    signal hmcad_x_clk                  : std_logic_vector(4 - 1 downto 0);
    signal hmcad_x_valid                : std_logic_vector(4 - 1 downto 0);
    signal hmcad_x_ready                : std_logic_vector(4 - 1 downto 0);
    signal hmcad_x_data                 : std_logic_vector(4*64 - 1 downto 0);
    signal hmcad_x_int                  : std_logic_vector(4 - 1 downto 0);

    signal qspi_x_clk                   : std_logic_vector(qspi_num_slave_port - 1 downto 0);
    signal qspi_x_cs_up                 : std_logic_vector(qspi_num_slave_port - 1 downto 0);
    signal qspi_x_ready                 : std_logic_vector(qspi_num_slave_port - 1 downto 0);
    signal qspi_x_data                  : std_logic_vector(qspi_num_slave_port*64 - 1 downto 0);

    signal fifo_full_out                : std_logic_vector(4 - 1 downto 0);
    signal fifo_empty_out               : std_logic_vector(4 - 1 downto 0);
    signal hmcad_rst_counter            : std_logic_vector(15 downto 0);
    signal hmcad_buffer_rst             : std_logic;

    signal adcx_lclk_p                  : std_logic_vector(3 downto 0);
    signal adcx_lclk_n                  : std_logic_vector(3 downto 0);
    signal adcx_fclk_p                  : std_logic_vector(3 downto 0);
    signal adcx_fclk_n                  : std_logic_vector(3 downto 0);
    signal adcx_dx_a_p                  : std_logic_vector(4*4 - 1 downto 0);
    signal adcx_dx_a_n                  : std_logic_vector(4*4 - 1 downto 0);
    signal adcx_dx_b_p                  : std_logic_vector(4*4 - 1 downto 0);
    signal adcx_dx_b_n                  : std_logic_vector(4*4 - 1 downto 0);


    signal fclk_qspi_data               : std_logic_vector(63 downto 0);
    signal fclk_qspi_cnt                : integer;
    signal fclk_qspi_rdy                : std_logic;
    signal fclk_qspi_rst                : std_logic;
    signal phaseAnalyzer_basedDCM_cont  : std_logic;
    signal phaseAnalyzer_basedDCM_result: std_logic_vector(16*(9+2*4)-1 downto 0);
    
    signal bitsleep_cnt                 : std_logic_vector(4*16 - 1 downto 0);
    
    

begin

rst <= infrst_rst_out;

process(clk_125MHz, rst)
begin
  if (rst = '1') then 
    hmcad_x4_block_rst <= '1';
    adcx_tick_ms_counter0 <= 0;
    adcx_tick_ms_counter1 <= 0;
    adcx_tick_ms_counter2 <= 0;
    adcx_tick_ms_counter3 <= 0;
    hmcad_rst_counter <= (others => '0');
  elsif rising_edge(clk_125MHz) then
    adcx_tick_ms_d0 <= adcx_tick_ms;
    adcx_tick_ms_d1 <= adcx_tick_ms_d0;
    
    if (SPIRegisters(SPIRegistersStrucrure'pos(ADCEnableReg))(0) = '1') then
      if (((adcx_tick_ms_d0(0) = '0') and adcx_tick_ms_d1(0) = '1') or ((adcx_tick_ms_d0(0) = '1') and adcx_tick_ms_d1(0) = '0')) then
        adcx_tick_ms_counter0 <= 0;
      else
        adcx_tick_ms_counter0 <= adcx_tick_ms_counter0 + 1;
      end if;
    else
      adcx_tick_ms_counter0 <= 0;
    end if;
    
    if (SPIRegisters(SPIRegistersStrucrure'pos(ADCEnableReg))(1) = '1') then
      if (((adcx_tick_ms_d0(1) = '0') and adcx_tick_ms_d1(1) = '1') or ((adcx_tick_ms_d0(1) = '1') and adcx_tick_ms_d1(1) = '0')) then
        adcx_tick_ms_counter1 <= 0;
      else
        adcx_tick_ms_counter1 <= adcx_tick_ms_counter1 + 1;
      end if;
    else
      adcx_tick_ms_counter1 <= 0;
    end if;
    
    if (SPIRegisters(SPIRegistersStrucrure'pos(ADCEnableReg))(2) = '1') then
      if (((adcx_tick_ms_d0(2) = '0') and adcx_tick_ms_d1(2) = '1') or ((adcx_tick_ms_d0(2) = '1') and adcx_tick_ms_d1(2) = '0')) then
        adcx_tick_ms_counter2 <= 0;
      else
        adcx_tick_ms_counter2 <= adcx_tick_ms_counter2 + 1;
      end if;
    else
      adcx_tick_ms_counter2 <= 0;
    end if;
    
    if (SPIRegisters(SPIRegistersStrucrure'pos(ADCEnableReg))(3) = '1') then
      if (((adcx_tick_ms_d0(3) = '0') and adcx_tick_ms_d1(3) = '1') or ((adcx_tick_ms_d0(3) = '1') and adcx_tick_ms_d1(3) = '0')) then
        adcx_tick_ms_counter3 <= 0;
      else
        adcx_tick_ms_counter3 <= adcx_tick_ms_counter3 + 1;
      end if;
    else
      adcx_tick_ms_counter3 <= 0;
    end if;
    
    if (spi_rst_cmd = '1') then 
      hmcad_x4_block_rst <= '1';
      hmcad_rst_counter <= (0 => '1', others => '0');
    elsif (adcx_tick_ms_counter0 > 2) then
      hmcad_x4_block_rst <= '1';
      hmcad_rst_counter <= hmcad_rst_counter + 1;
      adcx_tick_ms_counter0 <= 0;
    elsif (adcx_tick_ms_counter1 > 2) then
      hmcad_x4_block_rst <= '1';
      hmcad_rst_counter <= hmcad_rst_counter + 1;
      adcx_tick_ms_counter1 <= 0;
    elsif (adcx_tick_ms_counter2 > 2) then
      hmcad_x4_block_rst <= '1';
      hmcad_rst_counter <= hmcad_rst_counter + 1;
      adcx_tick_ms_counter2 <= 0;
    elsif (adcx_tick_ms_counter3 > 2) then
      hmcad_x4_block_rst <= '1';
      hmcad_rst_counter <= hmcad_rst_counter + 1;
      adcx_tick_ms_counter2 <= 0;
    else
      hmcad_x4_block_rst <= '0';
    end if;
  end if;
end process;

process(clk_125MHz)
begin
  if rising_edge(clk_125MHz) then
    adcx_calib_done_d <= adcx_calib_done;
    adcx_all_calib_done <= adcx_calib_done_d(3) and adcx_calib_done_d(2) and adcx_calib_done_d(1) and adcx_calib_done_d(0); 
    dd(1) <= adcx_all_calib_done;
    dd(0) <= pll_lock;
  end if;
end process;

dd(dd'length - 1 downto 2) <= (others => 'Z');

--controls_out(3 downto 0) <= adcx_fclk_out;
controls_out(3 downto 0) <= (others => 'Z');
controls_out(4) <= 'Z';


sys_rst <= (not xc_sys_rstn);

Clock_gen_inst : entity clock_generator
    Port map( 
      clk_in            => in_clk_20MHz,
      rst_in            => sys_rst,
      pll_lock          => pll_lock,
      clk_out_125MHz    => clk_125MHz,
      clk_out_250MHz    => clk_250MHz,
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

      m_fcb_clk         => clk_125MHz,
      m_fcb_areset      => infrst_rst_out,
      m_fcb_addr        => m_fcb_addr   ,
      m_fcb_wrdata      => m_fcb_wrdata ,
      m_fcb_wrreq       => m_fcb_wrreq  ,
      m_fcb_wrack       => m_fcb_wrack  ,
      m_fcb_rddata      => m_fcb_rddata ,
      m_fcb_rdreq       => m_fcb_rdreq  ,
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
  process(rst, clk_125MHz)
  begin
    if (rst = '1') then
      m_fcb_wrack <= '0';
      m_fcb_rdack <= '0';
    elsif rising_edge(clk_125MHz) then
      if (m_fcb_wrreq = '1') then
        m_fcb_wrack <= '1';
        SPIRegisters(conv_integer(m_fcb_addr)) <= m_fcb_wrdata;
      elsif (m_fcb_rdreq = '1') then
        m_fcb_rdack <= '1';
        m_fcb_rddata <= SPIRegisters(conv_integer(m_fcb_addr));
      else
        m_fcb_wrack <= '0';
        m_fcb_rdack <= '0';
        SPIRegisters(SPIRegistersStrucrure'pos(ControlReg))(ControlRegType'pos(StructureLength) - 1 downto 0) <= (others => '0');
        trigger_start_delay(0) <= '0';
      end if;
      spi_rst_cmd <= SPIRegisters(SPIRegistersStrucrure'pos(ControlReg))(ControlRegType'pos(program_rst));
      hmcad_buffer_rst <= SPIRegisters(SPIRegistersStrucrure'pos(ControlReg))(ControlRegType'pos(buffer_rst));
      SPIRegisters(SPIRegistersStrucrure'pos(BufferLength)) <= conv_std_logic_vector(c_max_num_data, 16);
--      SPIRegisters(SPIRegistersStrucrure'pos(HMCADCounter)) <= conv_std_logic_vector(dcm_state, 16);
      --SPIRegisters(SPIRegistersStrucrure'pos(HMCADCounter)) <= bitsleep_cnt;
      
      loop_proc : for i in 0 to 4 - 1 loop
        --if (phaseAnalyzer_basedDCM_cont = '1') then
        --  SPIRegisters(SPIRegistersStrucrure'pos(StructureLength) + i) <= phaseAnalyzer_basedDCM_result(i*(9+2*4) + (9+2*3) downto i*(9+2*4));
        --end if;
          SPIRegisters(SPIRegistersStrucrure'pos(StructureLength) + i) <= bitsleep_cnt(i*16 + 15 downto i*16);
          
        --SPIRegisters(SPIRegistersStrucrure'pos(StructureLength) + i) <= fclkInfo(i)(15 downto 0);
        --SPIRegisters(SPIRegistersStrucrure'pos(StructureLength) + i) <= conv_std_logic_vector(i, 16);
      end loop;

      
    end if;
  end process;



process(clk_125MHz, rst)
begin
  if (rst = '1') then
    trigger_start <= '0';
    trigger_mode <= "00";
  elsif rising_edge(clk_125MHz) then
    if (SPIRegisters(SPIRegistersStrucrure'pos(ControlReg))(ControlRegType'pos(mode_1) downto ControlRegType'pos(mode_0)) /= 0) then
      trigger_mode <= SPIRegisters(SPIRegistersStrucrure'pos(ControlReg))(ControlRegType'pos(mode_1) downto ControlRegType'pos(mode_0));
      trigger_start <= '1';
      trigger_start_counter <= 0;
    else
      if (trigger_start = '1') and (trigger_start_counter < 3) then
        trigger_start_counter <= trigger_start_counter + 1;
      else
        trigger_start <= '0';
      end if;
    end if;
    start_pulse <=  SPIRegisters(SPIRegistersStrucrure'pos(ControlReg))(ControlRegType'pos(pulse_start));
  end if;
end process;

pulse_proc :
  process(clk_125MHz)
  begin
    if rising_edge(clk_125MHz) then
      if (start_pulse = '1') then
        pulse_cnt <= (others => '0');
        pulse <= '1';
      else
        if (pulse = '1') then
          if (pulse_cnt(pulse_cnt'length - 1) = '1') then
            pulse <= '0';
          else
            pulse_cnt <= pulse_cnt + 1;
          end if;
        end if;
      end if;
    end if;
  end process;

process(hmcad_x_clk(0))
begin
  if rising_edge(hmcad_x_clk(0)) then
    pulse_sync_vec(0) <= pulse;
    pulse_sync_vec(pulse_sync_vec'length - 1 downto 1) <= pulse_sync_vec(pulse_sync_vec'length - 2 downto 0);
    pulse_sync <= (not pulse_sync_vec(pulse_sync_vec'length - 1)) and pulse_sync_vec(pulse_sync_vec'length - 2);
  end if;
end process;

pulse_out_proc : process(SPIRegisters(SPIRegistersStrucrure'pos(TriggerSetUp))(7 downto 0))
begin
  if (SPIRegisters(SPIRegistersStrucrure'pos(TriggerSetUp))(7) = '1') then
    pulse_out <= start_pulse;
  else
    pulse_out <= pulse_sync;
  end if;

  if (SPIRegisters(SPIRegistersStrucrure'pos(TriggerSetUp))(6) = '1') then
    pulse_p <= pulse_out;
    pulse_n <= not pulse_out;
  else
    pulse_p <= not pulse_out;
    pulse_n <= pulse_out;
  end if;
end process;

IBUFG_inst : IBUFG
generic map (
   IBUF_LOW_PWR => TRUE, -- Low power (TRUE) vs. performance (FALSE) setting for referenced I/O standards
   IOSTANDARD => "DEFAULT")
port map (
   O => spifi_sck_bufg, -- Clock buffer output
   I => spifi_sck  -- Clock buffer input (connect directly to top-level port)
);

adcx_lclk_p <= adc3_lclk_p & adc2_lclk_p & adc1_lclk_p & adc0_lclk_p;
adcx_lclk_n <= adc3_lclk_n & adc2_lclk_n & adc1_lclk_n & adc0_lclk_n;
adcx_fclk_p <= adc3_fclk_p & adc2_fclk_p & adc1_fclk_p & adc0_fclk_p;
adcx_fclk_n <= adc3_fclk_n & adc2_fclk_n & adc1_fclk_n & adc0_fclk_n;
adcx_dx_a_p <= adc3_dx_a_p & adc2_dx_a_p & adc1_dx_a_p & adc0_dx_a_p;
adcx_dx_a_n <= adc3_dx_a_n & adc2_dx_a_n & adc1_dx_a_n & adc0_dx_a_n;
adcx_dx_b_p <= adc3_dx_b_p & adc2_dx_b_p & adc1_dx_b_p & adc0_dx_b_p;
adcx_dx_b_n <= adc3_dx_b_n & adc2_dx_b_n & adc1_dx_b_n & adc0_dx_b_n;


hmcad_x4_block_inst : entity hmcad_x4_block
  Generic map (
    c_max_num_data         => c_max_num_data
  )
  Port map(
    areset                  => hmcad_x4_block_rst,
    TriggerSetUp            => SPIRegisters(SPIRegistersStrucrure'pos(TriggerSetUp)),
    ADCEnableReg            => SPIRegisters(SPIRegistersStrucrure'pos(ADCEnableReg)),
    TriggerPositionSetUp    => SPIRegisters(SPIRegistersStrucrure'pos(TriggerPositionSetUp)),
    mode                    => trigger_mode,
    start                   => trigger_start,
    
    adcx_lclk_p             => adcx_lclk_p,
    adcx_lclk_n             => adcx_lclk_n,
    adcx_fclk_p             => adcx_fclk_p,
    adcx_fclk_n             => adcx_fclk_n,
    adcx_dx_a_p             => adcx_dx_a_p,
    adcx_dx_a_n             => adcx_dx_a_n,
    adcx_dx_b_p             => adcx_dx_b_p,
    adcx_dx_b_n             => adcx_dx_b_n,
 
    slave_x_clk             => hmcad_x_clk  ,
    slave_x_valid           => hmcad_x_valid,
    slave_x_ready           => hmcad_x_ready,
    slave_x_data            => hmcad_x_data ,
    slave_x_cs_up           => qspi_x_cs_up(3 downto 0),
    
    bitsleep_cnt            => bitsleep_cnt,
    
    recorder_rst            => hmcad_buffer_rst,

    adcx_calib_done         => adcx_calib_done,
    adcx_interrupt          => hmcad_x_int,
    adcx_tick_ms            => adcx_tick_ms,
    
    adcx_fclk_out           => adcx_fclk_out

  );


--process(clk_125MHz)
--begin
--  if rising_edge(clk_125MHz) then
--    if (qspi_x_cs_up(qspi_x_cs_up'length - 1) = '1') then
--      fclk_qspi_cnt <= 0;
--    else
--      if (qspi_x_ready(qspi_x_ready'length - 1) = '1') then
--        fclk_qspi_cnt <= fclk_qspi_cnt + 1;
--      end if;
--    end if;
--    fclk_qspi_data(DCM_COUNTER'length+adcx_fclk_out'length*2-1 downto 0) <= fclkInfo(fclk_qspi_cnt);
--    fclk_qspi_data(fclk_qspi_data'length - 1 downto DCM_COUNTER'length+adcx_fclk_out'length*2) <= (others => '0');
--  end if;
--end process;

--process(clk_125MHz)
--begin
--  if rising_edge(clk_125MHz) then
--    if (qspi_x_cs_up(qspi_x_cs_up'length - 1) = '1') then
--      fclk_qspi_cnt <= 0;
--    else
--      if (qspi_x_ready(qspi_x_ready'length - 1) = '1') then
--        fclk_qspi_cnt <= fclk_qspi_cnt + 1;
--      end if;
--    end if;
--    fclk_qspi_data(DCM_COUNTER'length+adcx_fclk_out'length*2-1 downto 0) <= fclkInfo(fclk_qspi_cnt);
--    fclk_qspi_data(fclk_qspi_data'length - 1 downto DCM_COUNTER'length+adcx_fclk_out'length*2) <= conv_std_logic_vector(fclk_qspi_cnt, fclk_qspi_data'length - (DCM_COUNTER'length+adcx_fclk_out'length*2));
--  end if;
--end process;

--phaseAnalyzer_basedDCM_inst : entity phaseAnalyzer_basedDCM 
--    Generic map(
--      c_data_length     => 4,
--      c_num_out_state   => 16,
--      c_counter_width   => 9
--    )
--    Port map(
--      dcm_clk_in    => hmcad_x_clk(0),
--      clk_in        => clk_125MHz,
--      rst_in        => not adcx_all_calib_done,
--      continue_out  => phaseAnalyzer_basedDCM_cont,
--      data_in       => adcx_fclk_out,
--      result_out    => phaseAnalyzer_basedDCM_result
--    );

int_adcx <=  hmcad_x_int;
qspi_x_clk <= hmcad_x_clk;
hmcad_x_ready <= qspi_x_ready(3 downto 0);
qspi_x_data <= hmcad_x_data;


QSPI_interconnect_inst : entity QSPI_interconnect
  Generic map(
    c_num_slave_port    => qspi_num_slave_port,
    c_data_width        => 64,
    c_command_width     => 8,
    C_CPHA              => '0',
    C_CPOL              => '0',
    C_LSB_FIRST         => false
  )
  Port map(
    slave_x_clk         => qspi_x_clk,
    slave_x_ready       => qspi_x_ready,
    slave_x_data        => qspi_x_data ,
    slave_x_cs_up       => qspi_x_cs_up,
    qspi_sio            => spifi_sio,
    qspi_sck            => spifi_sck_bufg,
    qspi_cs             => spifi_cs
  );

end Behavioral;
