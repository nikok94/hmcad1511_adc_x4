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

    adcx_calib_done         : out std_logic_vector(3 downto 0);
    adcx_interrupt          : out std_logic_vector(3 downto 0);
    adcx_tick_ms            : out std_logic_vector(3 downto 0);
    
    slave_x_clk             : out std_logic_vector(4 - 1 downto 0);
    slave_x_valid           : out std_logic_vector(4 - 1 downto 0);
    slave_x_ready           : in std_logic_vector(4 - 1 downto 0);
    slave_x_data            : out std_logic_vector(4*c_data_width - 1 downto 0);
    slave_x_cs_up           : in std_logic_vector(4 - 1 downto 0)--;
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

  signal adc0_gclk                      : std_logic;
  signal adc0_gclk_out                  : std_logic;
  signal adc0_data                      : std_logic_vector(63 downto 0);
  signal rec0_data                      : std_logic_vector(63 downto 0);
  signal rec0_ready                     : std_logic;
  signal rec0_valid                     : std_logic;
  signal rec0_rst                       : std_logic;
  signal trigger0_enable                : std_logic;
  signal trigger0_out                   : std_logic;
  
  signal adc1_gclk                      : std_logic;
  signal adc1_gclk_out                  : std_logic;
  signal adc1_data                      : std_logic_vector(63 downto 0);
  signal rec1_data                      : std_logic_vector(63 downto 0);
  signal rec1_ready                     : std_logic;
  signal rec1_valid                     : std_logic;
  signal rec1_rst                       : std_logic;
  signal trigger1_enable                : std_logic;
  signal trigger1_out                   : std_logic;

  signal adc2_gclk                      : std_logic;
  signal adc2_gclk_out                  : std_logic;
  signal adc2_data                      : std_logic_vector(63 downto 0);
  signal rec2_data                      : std_logic_vector(63 downto 0);
  signal rec2_ready                     : std_logic;
  signal rec2_valid                     : std_logic;
  signal rec2_rst                       : std_logic;
  signal trigger2_enable                : std_logic;
  signal trigger2_out                   : std_logic;

  signal adc3_gclk                      : std_logic;
  signal adc3_gclk_out                  : std_logic;
  signal adc3_data                      : std_logic_vector(63 downto 0);
  signal rec3_data                      : std_logic_vector(63 downto 0);
  signal rec3_ready                     : std_logic;
  signal rec3_valid                     : std_logic;
  signal rec3_rst                       : std_logic;
  signal trigger3_enable                : std_logic;
  signal trigger3_out                   : std_logic;

  signal trigger                        : std_logic;
  signal rec0_irq                       : std_logic;
  signal rec1_irq                       : std_logic;
  signal rec2_irq                       : std_logic;
  signal rec3_irq                       : std_logic;


begin
trig_position <= TriggerPositionSetUp;
adcx_enable <= ADCEnableReg(3 downto 0);
mux_data_selector <= TriggerSetUp(3 downto 2);

process(mux_data_selector)
begin
  trigger0_enable <= '0';
  trigger1_enable <= '0';
  trigger2_enable <= '0';
  trigger3_enable <= '0';
  case(mux_data_selector) is
    when "00" =>
      trigger0_enable <= '1';
    when "01" => 
      trigger1_enable <= '1';
    when "10" =>
      trigger2_enable <= '1';
    when "11" =>
      trigger3_enable <= '1';
    when others =>
  end case;
end process;

trigger <= trigger0_out or trigger1_out or trigger2_out or trigger3_out;

adc0_inst : entity hmcad_adc_block
  Generic map(
    c_max_num_data        => c_max_num_data
  )
  Port map(
    trigger_enable        => trigger0_enable,
    trigger_condition     => TriggerSetUp(5 downto 4),
    trigger_level         => TriggerSetUp(15 downto 8),
    trigger_mode          => mode,
    trigger_set           => start,

    trigger_out           => trigger0_out,
    trigger_in            => trigger,

    lclk_p                => adc0_lclk_p,
    lclk_n                => adc0_lclk_n,
    fclk_p                => adc0_fclk_p,
    fclk_n                => adc0_fclk_n,
    dx_a_p                => adc0_dx_a_p,
    dx_a_n                => adc0_dx_a_n,
    dx_b_p                => adc0_dx_b_p,
    dx_b_n                => adc0_dx_b_n,
    areset                => areset,
    enable                => adcx_enable(0),
    gclk                  => adc0_gclk,
    gclk_out              => adc0_gclk_out,
    calib_done            => adcx_calib_done(0),
    tick_ms               => adcx_tick_ms(0),
    
    recorder_interrupt    => adcx_interrupt(0),
    recorder_rst          => slave_x_cs_up(0),
    recorder_data         => rec0_data,
    recorder_valid        => rec0_valid,
    recorder_ready        => slave_x_ready(0),
    recorder_offset       => trig_position(natural(round(log2(real(c_max_num_data))))-1 downto 0)
  );

adc1_inst : entity hmcad_adc_block
  Generic map(
    c_max_num_data        => c_max_num_data
  )
  Port map(
    trigger_enable        => trigger1_enable,
    trigger_condition     => TriggerSetUp(5 downto 4),
    trigger_level         => TriggerSetUp(15 downto 8),
    trigger_mode          => mode,
    trigger_set           => start,

    trigger_out           => trigger1_out,
    trigger_in            => trigger,

    lclk_p                => adc1_lclk_p,
    lclk_n                => adc1_lclk_n,
    fclk_p                => adc1_fclk_p,
    fclk_n                => adc1_fclk_n,
    dx_a_p                => adc1_dx_a_p,
    dx_a_n                => adc1_dx_a_n,
    dx_b_p                => adc1_dx_b_p,
    dx_b_n                => adc1_dx_b_n,
    areset                => areset,
    enable                => adcx_enable(1),
    gclk                  => adc1_gclk,
    gclk_out              => adc1_gclk_out,
    calib_done            => adcx_calib_done(1),
    tick_ms               => adcx_tick_ms(1),

    recorder_interrupt    => adcx_interrupt(1),
    recorder_rst          => slave_x_cs_up(1),
    recorder_data         => rec1_data,
    recorder_valid        => rec1_valid,
    recorder_ready        => slave_x_ready(1),
    recorder_offset       => trig_position(natural(round(log2(real(c_max_num_data))))-1 downto 0)
  );

adc2_inst : entity hmcad_adc_block
  Generic map(
    c_max_num_data        => c_max_num_data
  )
  Port map(
    trigger_enable        => trigger2_enable,
    trigger_condition     => TriggerSetUp(5 downto 4),
    trigger_level         => TriggerSetUp(15 downto 8),
    trigger_mode          => mode,
    trigger_set           => start,

    trigger_out           => trigger2_out,
    trigger_in            => trigger,

    lclk_p                => adc2_lclk_p,
    lclk_n                => adc2_lclk_n,
    fclk_p                => adc2_fclk_p,
    fclk_n                => adc2_fclk_n,
    dx_a_p                => adc2_dx_a_p,
    dx_a_n                => adc2_dx_a_n,
    dx_b_p                => adc2_dx_b_p,
    dx_b_n                => adc2_dx_b_n,
    areset                => areset,
    enable                => adcx_enable(2),
    gclk                  => adc2_gclk,
    gclk_out              => adc2_gclk_out,
    calib_done            => adcx_calib_done(2),
    tick_ms               => adcx_tick_ms(2),
    
    recorder_interrupt    => adcx_interrupt(2),
    recorder_rst          => slave_x_cs_up(2),
    recorder_data         => rec2_data,
    recorder_valid        => rec2_valid,
    recorder_ready        => slave_x_ready(2),
    recorder_offset       => trig_position(natural(round(log2(real(c_max_num_data))))-1 downto 0)
  );

adc3_inst : entity hmcad_adc_block
  Generic map(
    c_max_num_data        => c_max_num_data
  )
  Port map(
    trigger_enable        => trigger3_enable,
    trigger_condition     => TriggerSetUp(5 downto 4),
    trigger_level         => TriggerSetUp(15 downto 8),
    trigger_mode          => mode,
    trigger_set           => start,

    trigger_out           => trigger3_out,
    trigger_in            => trigger,

    lclk_p                => adc3_lclk_p,
    lclk_n                => adc3_lclk_n,
    fclk_p                => adc3_fclk_p,
    fclk_n                => adc3_fclk_n,
    dx_a_p                => adc3_dx_a_p,
    dx_a_n                => adc3_dx_a_n,
    dx_b_p                => adc3_dx_b_p,
    dx_b_n                => adc3_dx_b_n,
    areset                => areset,
    enable                => adcx_enable(3),
    gclk                  => adc3_gclk,
    gclk_out              => adc3_gclk_out,
    calib_done            => adcx_calib_done(3),
    tick_ms               => adcx_tick_ms(3),

    recorder_interrupt    => adcx_interrupt(3),
    recorder_rst          => slave_x_cs_up(3),
    recorder_data         => rec3_data,
    recorder_valid        => rec3_valid,
    recorder_ready        => slave_x_ready(3),
    recorder_offset       => trig_position(natural(round(log2(real(c_max_num_data))))-1 downto 0)
  );

adc0_gclk <= adc0_gclk_out;
adc1_gclk <= adc0_gclk_out;
adc2_gclk <= adc0_gclk_out;
adc3_gclk <= adc0_gclk_out;

--adc0_gclk <= adc0_gclk_out;
--adc1_gclk <= adc1_gclk_out;
--adc2_gclk <= adc2_gclk_out;
--adc3_gclk <= adc3_gclk_out;

--spifi_sio <= PCS_O when spifi_T = '0' else (others => 'Z');
--
--PCS_I <= spifi_sio;

-- spifi_switch_byte_switch_proc :
--   process(spifi_sck, spifi_cs)
--   begin
--     if (spifi_cs = '1') then 
--       spifi_T <= '1';
--       spifi_cmd_counter <= '0';
--     elsif rising_edge(spifi_sck) then
--       if (spifi_cmd_valid = '1') then
-- 
--         if (spifi_cmd_counter = '0') then
--           spifi_cmd_counter <= '1';
--           spifi_switch_byte <= spifi_cmd_byte;
--           spifi_T <= '0'; 
--         end if;
-- 
--       end if;
--     end if;
--   end process;
--

--QSPI_interconnect_inst : entity QSPI_interconnect
--  Generic map(
--    c_num_slave_port    => 4,
--    c_data_width        => c_data_width,
--    c_command_width     => 8,
--    C_CPHA              => '0',
--    C_CPOL              => '0',
--    C_LSB_FIRST         => true
--  )
--  Port map(
--    slave_x_clk         => slave_x_clk  ,
--    slave_x_ready       => slave_x_ready,
--    slave_x_data        => slave_x_data ,
--    slave_x_cs_up       => slave_x_cs_up,
--    qspi_sio            => spifi_sio,
--    qspi_sck            => spifi_sck,
--    qspi_cs             => spifi_cs
--  );

slave_x_valid <= rec3_valid & rec2_valid & rec1_valid & rec0_valid;
slave_x_clk <= adc3_gclk & adc2_gclk & adc1_gclk & adc0_gclk;
slave_x_data <= rec3_data & rec2_data & rec1_data & rec0_data;


--spifi_mux_data_process :
--  process (spifi_switch_byte, rec0_data, rec1_data, rec2_data, rec3_data, spifi_cs, spifi_s_ready, spifi_T)
--  begin
--    rec0_rst <= '0';
--    rec1_rst <= '0';
--    rec2_rst <= '0';
--    rec3_rst <= '0';
--    rec0_ready <= '0';
--    rec1_ready <= '0';
--    rec2_ready <= '0';
--    rec3_ready <= '0';
--     case spifi_switch_byte is
--        when x"00" => spifi_s_data <= rec0_data;
--                      --spifi_s_valid <= rec0_valid;
--                      rec0_rst <= spifi_cs;
--                      rec0_ready <= spifi_s_ready and (not spifi_T);
--        when x"01" => spifi_s_data <= rec1_data;
--                      --spifi_s_valid <= rec1_valid;
--                      rec1_rst <= spifi_cs;
--                      rec1_ready <= spifi_s_ready  and (not spifi_T);
--        when x"02" => spifi_s_data <= rec2_data;
--                      --spifi_s_valid <= rec2_valid;
--                      rec2_rst <= spifi_cs;
--                      rec2_ready <= spifi_s_ready  and (not spifi_T);
--        when x"03" => spifi_s_data <= rec3_data;
--                      --spifi_s_valid <= rec3_valid;
--                      rec3_rst <= spifi_cs;
--                      rec3_ready <= spifi_s_ready  and (not spifi_T);
--        when others => spifi_s_data <= rec0_data;
--                       --spifi_s_valid <= rec0_valid;
--                       rec0_rst <= spifi_cs;
--                       rec0_ready <= spifi_s_ready  and (not spifi_T);
--     end case;
--  end process;
--
--spifi_module_inst : entity spifi_module
--    generic map(
--      C_CPHA            => '0',
--      C_CPOL            => '0',
--      C_LSB_FIRST       => true,
--      C_NUM_QBURST      => C_BURST_WIDTH_SPIFI
--    )
--    port map( 
--      SCK               => spifi_sck,
--      CS                => spifi_cs,
--
--      PCS_I             => PCS_I,
--      PCS_O             => PCS_O,
--
--      s_data            => spifi_s_data,
----      s_valid           => spifi_s_valid,
--      s_ready           => spifi_s_ready,
--
--      cmd_byte          => spifi_cmd_byte,
--      cmd_valid         => spifi_cmd_valid
--    );
end Behavioral;
