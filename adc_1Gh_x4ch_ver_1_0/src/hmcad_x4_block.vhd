----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 28.12.2019 10:05:15
-- Design Name: 
-- Module Name: hmcad_x4_block - Behavioral
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
use IEEE.math_real.ALL;
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
library UNISIM;
use UNISIM.VComponents.all;

library work;
use work.serdes_1_to_n_clk_ddr_s8_diff;
use work.serdes_1_to_n_data_ddr_s8_diff;
--use work.spifi_module;
use work.QSPI_interconnect;
use work.trigger_capture;
use work.data_recorder;
use work.hmcad_adc_block;
--use work.HMCAD1511_v3_00;

entity hmcad_x4_block is
  generic (
    c_max_num_data          : integer := 128;
    c_data_width            : integer := 64
  );
  Port (
    areset                  : in std_logic;
    TriggerSetUp            : in std_logic_vector(15 downto 0);
    ADCEnableReg            : in std_logic_vector(15 downto 0);
    TriggerPositionSetUp    : in std_logic_vector(15 downto 0);
    mode                    : in std_logic_vector(1 downto 0);
    start                   : in std_logic;
    adcx_lclk_p             : in std_logic_vector(3 downto 0);
    adcx_lclk_n             : in std_logic_vector(3 downto 0);
    adcx_fclk_p             : in std_logic_vector(3 downto 0);
    adcx_fclk_n             : in std_logic_vector(3 downto 0);
    adcx_dx_a_p             : in std_logic_vector(4*4 - 1 downto 0);
    adcx_dx_a_n             : in std_logic_vector(4*4 - 1 downto 0);
    adcx_dx_b_p             : in std_logic_vector(4*4 - 1 downto 0);
    adcx_dx_b_n             : in std_logic_vector(4*4 - 1 downto 0);
--    adc0_lclk_p             : in std_logic;
--    adc0_lclk_n             : in std_logic;
--    adc0_fclk_p             : in std_logic;
--    adc0_fclk_n             : in std_logic;
--    adc0_dx_a_p             : in std_logic_vector(3 downto 0);
--    adc0_dx_a_n             : in std_logic_vector(3 downto 0);
--    adc0_dx_b_p             : in std_logic_vector(3 downto 0);
--    adc0_dx_b_n             : in std_logic_vector(3 downto 0);
--
--    adc1_lclk_p             : in std_logic;
--    adc1_lclk_n             : in std_logic;
--    adc1_fclk_p             : in std_logic;
--    adc1_fclk_n             : in std_logic;
--    adc1_dx_a_p             : in std_logic_vector(3 downto 0);
--    adc1_dx_a_n             : in std_logic_vector(3 downto 0);
--    adc1_dx_b_p             : in std_logic_vector(3 downto 0);
--    adc1_dx_b_n             : in std_logic_vector(3 downto 0);
--
--    adc2_lclk_p             : in std_logic;
--    adc2_lclk_n             : in std_logic;
--    adc2_fclk_p             : in std_logic;
--    adc2_fclk_n             : in std_logic;
--    adc2_dx_a_p             : in std_logic_vector(3 downto 0);
--    adc2_dx_a_n             : in std_logic_vector(3 downto 0);
--    adc2_dx_b_p             : in std_logic_vector(3 downto 0);
--    adc2_dx_b_n             : in std_logic_vector(3 downto 0);
--
--    adc3_lclk_p             : in std_logic;
--    adc3_lclk_n             : in std_logic;
--    adc3_fclk_p             : in std_logic;
--    adc3_fclk_n             : in std_logic;
--    adc3_dx_a_p             : in std_logic_vector(3 downto 0);
--    adc3_dx_a_n             : in std_logic_vector(3 downto 0);
--    adc3_dx_b_p             : in std_logic_vector(3 downto 0);
--    adc3_dx_b_n             : in std_logic_vector(3 downto 0);

    adcx_calib_done         : out std_logic_vector(3 downto 0);
    adcx_interrupt          : out std_logic_vector(3 downto 0);
    adcx_tick_ms            : out std_logic_vector(3 downto 0);
    
    slave_x_clk             : out std_logic_vector(4 - 1 downto 0);
    slave_x_valid           : out std_logic_vector(4 - 1 downto 0);
    slave_x_ready           : in std_logic_vector(4 - 1 downto 0);
    slave_x_data            : out std_logic_vector(4*c_data_width - 1 downto 0);
    slave_x_cs_up           : in std_logic_vector(4 - 1 downto 0);
    
    recorder_rst            : in std_logic
    --;
--    
--    
--    spifi_cs                : in std_logic;
--    spifi_sck               : in std_logic;
--    spifi_sio               : inout std_logic_vector(3 downto 0)

    );
end hmcad_x4_block;

architecture Behavioral of hmcad_x4_block is
  constant C_BURST_WIDTH_SPIFI          : integer := 16;
  constant num_data                     : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0) := (others => '0');
  constant bitslip_delay                : integer := 10;
  constant calid_done_delay             : integer := 10000000;

  signal reg_address_int                : integer;
  
  signal mux_data                       : std_logic_vector(63 downto 0);
  signal mux_data_selector              : std_logic_vector(1 downto 0);
  
  signal spifi_T                        : std_logic;
  signal spifi_cmd_counter              : std_logic;
  signal spifi_switch_byte              : std_logic_vector(7 downto 0);
  
  signal spifi_cmd_byte                 : std_logic_vector(7 downto 0);
  signal spifi_cmd_valid                : std_logic;
  
  signal PCS_I                          : std_logic_vector(3 downto 0);
  signal PCS_O                          : std_logic_vector(3 downto 0);
  signal spifi_s_data                   : std_logic_vector(C_BURST_WIDTH_SPIFI*4 - 1 downto 0);
  signal spifi_s_valid                  : std_logic;
  signal spifi_s_ready                  : std_logic;
  signal spifi_s_ready_sync             : std_logic;
  signal spifi_s_ready_dvec             : std_logic_vector(1 downto 0);
  signal spifi_cs_dvec                  : std_logic_vector(2 downto 0);
  signal spifi_cs_up                    : std_logic;
  
  signal trig_clk                       : std_logic;
  signal trigger_start                  : std_logic;
  signal capture_mode                   : std_logic_vector(1 downto 0);
  signal front_condition                : std_logic_vector(1 downto 0);
  signal capture_level                  : std_logic_vector(7 downto 0);
  signal start_offset                   : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);

  signal state                          : integer;
  constant frame_sync_pattern           : std_logic_vector(7 downto 0) := x"0F";
  signal adcx_enable                    : std_logic_vector(3 downto 0) := "1111";
  signal TriggerSetUp_d                 : std_logic_vector(15 downto 0);
  signal ADCEnableReg_d                 : std_logic_vector(15 downto 0);
  signal TriggerPositionSetUp_d         : std_logic_vector(15 downto 0);
  signal mode_d                         : std_logic_vector(1 downto 0);
  signal trig_position                  : std_logic_vector(15 downto 0);
  signal tick_counter                   : integer;
  signal adcx_calib_status              : std_logic_vector(3 downto 0);
  signal start_dvec                     : std_logic_vector(3 downto 0);
  signal start_sync                     : std_logic;
  
  signal adcx_gclk                      : std_logic_vector(3 downto 0);
  signal adcx_gclk_out                  : std_logic_vector(3 downto 0);
  type x4datatype   is array(3 downto 0) of std_logic_vector(63 downto 0);
  signal recx_data                      : x4datatype;
  signal recx_ready                     : std_logic_vector(3 downto 0);
  signal recx_valid                     : std_logic_vector(3 downto 0);
  signal recx_rst                       : std_logic_vector(3 downto 0);
  signal trigger_enable                 : std_logic_vector(3 downto 0);
  signal trigger_out                    : std_logic_vector(3 downto 0);
  
  signal adcx_clkrxioclkp_out           : std_logic_vector(3 downto 0);
  signal adcx_clkrxioclkn_out           : std_logic_vector(3 downto 0);
  signal adcx_clkrx_serdesstrobe_out    : std_logic_vector(3 downto 0);
  signal adcx_clkrxioclkp_in            : std_logic_vector(3 downto 0);
  signal adcx_clkrxioclkn_in            : std_logic_vector(3 downto 0);
  signal adcx_clkrx_serdesstrobe_in     : std_logic_vector(3 downto 0);

  signal adcx_clkdly_m_out              : std_logic_vector(3 downto 0);
  signal adcx_clkdly_s_out              : std_logic_vector(3 downto 0);
  signal adcx_clkdly_m_in               : std_logic_vector(3 downto 0);
  signal adcx_clkdly_s_in               : std_logic_vector(3 downto 0);
  signal trigger                        : std_logic;
  signal rec0_irq                       : std_logic;
  signal rec1_irq                       : std_logic;
  signal rec2_irq                       : std_logic;
  signal rec3_irq                       : std_logic;
  signal recorder_rst_vec               : std_logic_vector(3 downto 0);


begin
trig_position <= TriggerPositionSetUp;
adcx_enable <= ADCEnableReg(3 downto 0);
mux_data_selector <= TriggerSetUp(3 downto 2);

adc_block_gen : for i in 0 to recorder_rst_vec'length - 1 generate
  recorder_rst_vec(i) <= slave_x_cs_up(i) or recorder_rst;
  
  adc0_inst : entity hmcad_adc_block
  Generic map(
    c_max_num_data        => c_max_num_data
  )
  Port map(
    trigger_enable        => trigger_enable(i),
    trigger_condition     => TriggerSetUp(5 downto 4),
    trigger_level         => TriggerSetUp(15 downto 8),
    trigger_mode          => mode,
    trigger_set           => start,

    trigger_out           => trigger_out(i),
    trigger_in            => trigger,

    lclk_p                => adcx_lclk_p(i),
    lclk_n                => adcx_lclk_n(i),
    fclk_p                => adcx_fclk_p(i),
    fclk_n                => adcx_fclk_n(i),
    dx_a_p                => adcx_dx_a_p(i*4 + 3 downto i*4),
    dx_a_n                => adcx_dx_a_n(i*4 + 3 downto i*4),
    dx_b_p                => adcx_dx_b_p(i*4 + 3 downto i*4),
    dx_b_n                => adcx_dx_b_n(i*4 + 3 downto i*4),
    areset                => areset,
    enable                => adcx_enable(i),
    gclk                  => adcx_gclk(i),
    gclk_out              => adcx_gclk_out(i),
    clkrxioclkp_out       => adcx_clkrxioclkp_out(i),
    clkrxioclkn_out       => adcx_clkrxioclkn_out(i),
    clkrx_serdesstrobe_out=> adcx_clkrx_serdesstrobe_out(i),
    clkrxioclkp_in        => adcx_clkrxioclkp_in(i),
    clkrxioclkn_in        => adcx_clkrxioclkn_in(i),
    clkrx_serdesstrobe_in => adcx_clkrx_serdesstrobe_in(i),
    
    clkdly_m_out          => adcx_clkdly_m_out(i),
    clkdly_s_out          => adcx_clkdly_s_out(i),
    clkdly_m_in           => adcx_clkdly_m_in(i),
    clkdly_s_in           => adcx_clkdly_s_in(i),

    calib_done            => adcx_calib_done(i),
    tick_ms               => adcx_tick_ms(i),
    
    recorder_interrupt    => adcx_interrupt(i),
    recorder_rst          => recorder_rst_vec(i),
    recorder_data         => recx_data(i),
    recorder_valid        => recx_valid(i),
    recorder_ready        => slave_x_ready(i),
    recorder_offset       => trig_position(natural(round(log2(real(c_max_num_data))))-1 downto 0)
  );
  
end generate;

process(mux_data_selector)
begin
  trigger_enable <= (others => '0');
  case(mux_data_selector) is
    when "00" =>
      trigger_enable(0) <= '1';
    when "01" => 
      trigger_enable(1) <= '1';
    when "10" =>
      trigger_enable(2) <= '1';
    when "11" =>
      trigger_enable(3) <= '1';
    when others =>
  end case;
end process;

trigger <= trigger_out(0) or trigger_out(1) or trigger_out(2) or trigger_out(3);


adcx_gclk(0) <= adcx_gclk_out(0);
adcx_gclk(1) <= adcx_gclk_out(1);
adcx_gclk(2) <= adcx_gclk_out(2);
adcx_gclk(3) <= adcx_gclk_out(3);

adcx_clkrxioclkp_in(0) <= adcx_clkrxioclkp_out(0);
adcx_clkrxioclkn_in(0) <= adcx_clkrxioclkn_out(0);
adcx_clkrx_serdesstrobe_in(0) <= adcx_clkrx_serdesstrobe_out(0);

adcx_clkrxioclkp_in(1) <= adcx_clkrxioclkp_out(1);
adcx_clkrxioclkn_in(1) <= adcx_clkrxioclkn_out(1);
adcx_clkrx_serdesstrobe_in(1) <= adcx_clkrx_serdesstrobe_out(1);

adcx_clkrxioclkp_in(2) <= adcx_clkrxioclkp_out(2);
adcx_clkrxioclkn_in(2) <= adcx_clkrxioclkn_out(2);
adcx_clkrx_serdesstrobe_in(2) <= adcx_clkrx_serdesstrobe_out(2);

adcx_clkrxioclkp_in(3) <= adcx_clkrxioclkp_out(3);
adcx_clkrxioclkn_in(3) <= adcx_clkrxioclkn_out(3);
adcx_clkrx_serdesstrobe_in(3) <= adcx_clkrx_serdesstrobe_out(3);


adcx_clkdly_m_in(0) <= adcx_clkdly_m_out(0);
adcx_clkdly_s_in(0) <= adcx_clkdly_s_out(0);

adcx_clkdly_m_in(1) <= adcx_clkdly_m_out(1);
adcx_clkdly_s_in(1) <= adcx_clkdly_s_out(1);

adcx_clkdly_m_in(2) <= adcx_clkdly_m_out(2);
adcx_clkdly_s_in(2) <= adcx_clkdly_s_out(2);

adcx_clkdly_m_in(3) <= adcx_clkdly_m_out(3);
adcx_clkdly_s_in(3) <= adcx_clkdly_s_out(3);

slave_x_valid <= recx_valid;
slave_x_clk <= adcx_gclk;
slave_x_data <= recx_data(3) & recx_data(2) & recx_data(1) & recx_data(0);

end Behavioral;
