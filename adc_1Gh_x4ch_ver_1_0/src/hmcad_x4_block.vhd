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
use work.spifi_module;
use work.trigger_capture;
use work.data_recorder;
--use work.HMCAD1511_v3_00;

entity hmcad_x4_block is
  generic (
    c_max_num_data          : integer := 128
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
    adcx_data_valid         : out std_logic_vector(3 downto 0);
    
    adcx_valid              : out std_logic_vector(3 downto 0);
    
    state_out               : out integer;
    
    spifi_cs                : in std_logic;
    spifi_sck               : in std_logic;
    spifi_sio               : inout std_logic_vector(3 downto 0);
    
    trigger_start_out       : out std_logic

    );
end hmcad_x4_block;

architecture Behavioral of hmcad_x4_block is
  constant C_BURST_WIDTH_SPIFI        : integer := 16;
  constant num_data                   : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0) := (others => '0');
  constant c_data_width               : integer := 64;
  constant bitslip_delay                    : integer := 10;
  constant calid_done_delay                 : integer := 10000000;

  signal adc0_deser_clk_rxioclkp            : std_logic;
  signal adc0_deser_clkrxioclkn             : std_logic;
  signal adc0_deser_clkrx_serdesstrobe      : std_logic;
  signal adc0_deser_clkrx_bufg_x1           : std_logic;
  signal adc0_deser_bitslip                 : std_logic;
  signal adc0_deser_data_out                : std_logic_vector(9*8 - 1 downto 0);
  signal adc0_deser_datain_p                : std_logic_vector(8 downto 0);
  signal adc0_deser_datain_n                : std_logic_vector(8 downto 0);
  
  signal adc0_valid                         : std_logic;
  signal adc0_data                          : std_logic_vector(63 downto 0);
  signal adc0_frame                         : std_logic_vector(7 downto 0);
  signal rec0_m_data                        : std_logic_vector(c_data_width - 1 downto 0);
  signal rec0_m_valid                       : std_logic;
  signal rec0_m_ready                       : std_logic;
  signal rec0_rst                           : std_logic;
  signal rec0_rst_sync_vec                  : std_logic_vector(2 downto 0);
  signal rec0_data_valid                    : std_logic;
  signal rec0_ready_dvec                    : std_logic_vector(1 downto 0);
  signal rec0_rst_all                       : std_logic;

  signal adc1_deser_clk_rxioclkp            : std_logic;
  signal adc1_deser_clkrxioclkn             : std_logic;
  signal adc1_deser_clkrx_serdesstrobe      : std_logic;
  signal adc1_deser_clkrx_bufg_x1           : std_logic;
  signal adc1_deser_bitslip                 : std_logic;
  signal adc1_deser_data_out                : std_logic_vector(9*8 - 1 downto 0);
  signal adc1_deser_datain_p                : std_logic_vector(8 downto 0);
  signal adc1_deser_datain_n                : std_logic_vector(8 downto 0);
  signal adc1_valid                         : std_logic;
  signal adc1_data                          : std_logic_vector(63 downto 0);
  signal adc1_frame                         : std_logic_vector(7 downto 0);
  signal rec1_m_data                        : std_logic_vector(c_data_width - 1 downto 0);
  signal rec1_m_valid                       : std_logic;
  signal rec1_m_ready                       : std_logic;
  signal rec1_rst                           : std_logic;
  signal rec1_rst_sync_vec                  : std_logic_vector(2 downto 0);
  signal rec1_data_valid                    : std_logic;
  signal rec1_ready_dvec                    : std_logic_vector(1 downto 0);
  signal rec1_rst_all                       : std_logic;

  signal adc2_deser_clk_rxioclkp            : std_logic;
  signal adc2_deser_clkrxioclkn             : std_logic;
  signal adc2_deser_clkrx_serdesstrobe      : std_logic;
  signal adc2_deser_clkrx_bufg_x1           : std_logic;
  signal adc2_deser_bitslip                 : std_logic;
  signal adc2_deser_data_out                : std_logic_vector(9*8 - 1 downto 0);
  signal adc2_deser_datain_p                : std_logic_vector(8 downto 0);
  signal adc2_deser_datain_n                : std_logic_vector(8 downto 0);
  signal adc2_valid                         : std_logic;
  signal adc2_data                          : std_logic_vector(63 downto 0);
  signal adc2_frame                         : std_logic_vector(7 downto 0);
  signal rec2_m_data                        : std_logic_vector(c_data_width - 1 downto 0);
  signal rec2_m_valid                       : std_logic;
  signal rec2_m_ready                       : std_logic;
  signal rec2_rst                           : std_logic;
  signal rec2_rst_sync_vec                  : std_logic_vector(2 downto 0);
  signal rec2_data_valid                    : std_logic;
  signal rec2_ready_dvec                    : std_logic_vector(1 downto 0);
  signal rec2_rst_all                       : std_logic;

  signal adc3_deser_clk_rxioclkp            : std_logic;
  signal adc3_deser_clkrxioclkn             : std_logic;
  signal adc3_deser_clkrx_serdesstrobe      : std_logic;
  signal adc3_deser_clkrx_bufg_x1           : std_logic;
  signal adc3_deser_bitslip                 : std_logic;
  signal adc3_deser_data_out                : std_logic_vector(9*8 - 1 downto 0);
  signal adc3_deser_datain_p                : std_logic_vector(8 downto 0);
  signal adc3_deser_datain_n                : std_logic_vector(8 downto 0);

  signal adc3_data                          : std_logic_vector(63 downto 0);
  signal adc3_frame                         : std_logic_vector(7 downto 0);
  signal rec3_m_data                        : std_logic_vector(c_data_width - 1 downto 0);
  signal rec3_m_valid                       : std_logic;
  signal rec3_m_ready                       : std_logic;
  signal rec3_rst                           : std_logic;
  signal rec3_rst_sync_vec                  : std_logic_vector(2 downto 0);
  signal rec3_data_valid                    : std_logic;
  signal rec3_ready_dvec                    : std_logic_vector(1 downto 0);
  signal rec3_rst_all                       : std_logic;

  signal reg_address_int              : integer;
  
  signal mux_data                     : std_logic_vector(63 downto 0);
  signal mux_data_selector            : std_logic_vector(1 downto 0);
  
  signal spifi_T                      : std_logic;
  signal spifi_cmd_counter            : std_logic;
  signal spifi_switch_byte            : std_logic_vector(7 downto 0);
  
  signal spifi_cmd_byte               : std_logic_vector(7 downto 0);
  signal spifi_cmd_valid              : std_logic;
  
  signal PCS_I                        : std_logic_vector(3 downto 0);
  signal PCS_O                        : std_logic_vector(3 downto 0);
  signal spifi_s_data                 : std_logic_vector(C_BURST_WIDTH_SPIFI*4 - 1 downto 0);
  signal spifi_s_valid                : std_logic;
  signal spifi_s_ready                : std_logic;
  signal spifi_s_ready_sync           : std_logic;
  signal spifi_s_ready_dvec           : std_logic_vector(1 downto 0);
  signal spifi_cs_dvec                : std_logic_vector(2 downto 0);
  signal spifi_cs_up                  : std_logic;
  
  signal trig_clk                     : std_logic;
  signal trigger_start                : std_logic;
  signal capture_mode                 : std_logic_vector(1 downto 0);
  signal front_condition              : std_logic_vector(1 downto 0);
  signal capture_level                : std_logic_vector(7 downto 0);
  signal start_offset                 : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);

  signal state                        : integer;
  signal gclk                         : std_logic;
  constant frame_sync_pattern         : std_logic_vector(7 downto 0) := x"0F";
  signal adcx_enable                  : std_logic_vector(3 downto 0) := "1111";
  signal TriggerSetUp_d               : std_logic_vector(15 downto 0);
  signal ADCEnableReg_d               : std_logic_vector(15 downto 0);
  signal TriggerPositionSetUp_d       : std_logic_vector(15 downto 0);
  signal mode_d                       : std_logic_vector(1 downto 0);
  signal trig_position                : std_logic_vector(15 downto 0);
  signal tick_counter                 : integer;
  signal adc0_valid_counter           : integer;
  signal adc1_valid_counter           : integer;
  signal adc2_valid_counter           : integer;
  signal adc3_valid_counter           : integer;
  signal adcx_calib_status            : std_logic_vector(3 downto 0);
  signal start_dvec                   : std_logic_vector(3 downto 0);
  signal start_sync                   : std_logic;


begin

gclk <= adc0_deser_clkrx_bufg_x1;

adcx_calib_done <= adcx_calib_status;

trigger_start_out <= trigger_start;

sync_proc :
process(gclk)
begin
  if rising_edge(gclk) then
    TriggerSetUp_d         <= TriggerSetUp;
    ADCEnableReg_d         <= ADCEnableReg;
    TriggerPositionSetUp_d <= TriggerPositionSetUp;
    mode_d <= mode;
    start_dvec(0) <= start;
    start_dvec(start_dvec'length - 1 downto 1) <= start_dvec(start_dvec'length - 2 downto 0);
    start_sync <= (not start_dvec(start_dvec'length - 1)) and start_dvec(start_dvec'length - 2);
    adcx_enable <= ADCEnableReg_d(3 downto 0);
    mux_data_selector <= TriggerSetUp_d(3 downto 2);
    front_condition <= TriggerSetUp_d(5 downto 4);
    capture_level <= TriggerSetUp_d(15 downto 8);
    capture_mode <= mode_d;
    trig_position <= TriggerPositionSetUp_d;
  end if;
end process;

state_out <= state;

process(areset, gclk)
begin
  if (areset = '1') then
    state <= 0;
    adc0_deser_bitslip <= '0';
    adc1_deser_bitslip <= '0';
    adc2_deser_bitslip <= '0';
    adc3_deser_bitslip <= '0';
    adcx_calib_status <= (others => '0');
  elsif rising_edge(gclk) then
    case state is
      when 0 =>
        state <= 1;
        adc0_deser_bitslip <= '0';
        adc1_deser_bitslip <= '0';
        adc2_deser_bitslip <= '0';
        adc3_deser_bitslip <= '0';
        adcx_calib_status <= (others => '0');
      when 1 =>
        if (adcx_enable(0) = '1') then
          if (adc0_frame = frame_sync_pattern) then
            state <= 3;
            adcx_calib_status(0) <= '1';
          else
            state <= 2;
            adc0_deser_bitslip <= '1';
            tick_counter <= 0;
          end if;
        else
          state <= 3;
          adcx_calib_status(0) <= '1';
        end if;
      when 2 =>
        adc0_deser_bitslip <= '0';
        if (adc0_deser_bitslip = '0') then
          if (tick_counter < bitslip_delay) then
            tick_counter <= tick_counter + 1;
          else
            state <= 1;
          end if;
        end if;
      when 3 =>
        if (adcx_enable(1) = '1') then
          if (adc1_frame = frame_sync_pattern) then
            state <= 5;
            adcx_calib_status(1) <= '1';
          else
            state <= 4;
            adc1_deser_bitslip <= '1';
            tick_counter <= 0;
          end if;
        else 
          state <= 5;
          adcx_calib_status(1) <= '1';
        end if;
      when 4 =>
        adc1_deser_bitslip <= '0';
        
        if (adc1_deser_bitslip = '0') then
          if (tick_counter < bitslip_delay) then
            tick_counter <= tick_counter + 1;
          else
            state <= 3;
          end if;
        end if;
      when 5 =>
        if (adcx_enable(2) = '1') then
          if (adc2_frame = frame_sync_pattern) then
            state <= 7;
            adcx_calib_status(2) <= '1';
          else
            state <= 6;
            adc2_deser_bitslip <= '1';
            tick_counter <= 0;
          end if;
        else
          state <= 7;
          adcx_calib_status(2) <= '1';
        end if;
      when 6 =>
        adc2_deser_bitslip <= '0';
        if (adc2_deser_bitslip = '0') then
          if (tick_counter < bitslip_delay) then
            tick_counter <= tick_counter + 1;
          else
            state <= 5;
          end if;
        end if;
      when 7 =>
        if (adcx_enable(3) = '1') then
          if (adc3_frame = frame_sync_pattern) then
              state <= 9;
              adcx_calib_status(3) <= '1';
              tick_counter <= 0;
          else
              state <= 8;
              adc3_deser_bitslip <= '1';
              tick_counter <= 0;
          end if;
        else
            state <= 9;
            adcx_calib_status(3) <= '1';
        end if;
      when 8 =>
        adc3_deser_bitslip <= '0';
        
        if (adc3_deser_bitslip = '0') then
          if (tick_counter < bitslip_delay) then
            tick_counter <= tick_counter + 1;
          else
            state <= 7;
          end if;
        end if;
      when 9 =>
        if (tick_counter < calid_done_delay) then
          tick_counter <= tick_counter + 1;
        else 
          state <= 10;
        end if;
        adc0_deser_bitslip <= '0';
        adc1_deser_bitslip <= '0';
        adc2_deser_bitslip <= '0';
        adc3_deser_bitslip <= '0';
        adc0_valid_counter <= 0;
        adc1_valid_counter <= 0;
        adc2_valid_counter <= 0;
        adc3_valid_counter <= 0;
      when 10 =>
        if (adcx_calib_status /= "1111") then
          state <= 0;
        end if;
        
        if (adcx_enable(0) = '1') then 
          if (adc0_frame /= frame_sync_pattern) then
            adcx_calib_status(0) <= '0';
          else 
            adc0_valid_counter <= 0;
          end if;
        end if;
        
        if (adcx_enable(1) = '1') then 
          if (adc1_frame /= frame_sync_pattern) then
            adcx_calib_status(1) <= '0';
          else 
            adc1_valid_counter <= 0;
          end if;
        end if;
        
        if (adcx_enable(2) = '1') then 
          if (adc2_frame /= frame_sync_pattern) then
            adcx_calib_status(2) <= '0';
          else 
            adc2_valid_counter <= 0;
          end if;
        end if;
        
        if (adcx_enable(3) = '1') then
          if (adc3_frame /= frame_sync_pattern) then
            adcx_calib_status(3) <= '0';
          else 
            adc3_valid_counter <= 0;
          end if;
        end if;
      when others =>
        state <= 0;
    end case;
  end if;
end process;

mux_data_process :
  process (mux_data_selector, adc0_data, adc1_data, adc2_data, adc3_data)
  begin
     case mux_data_selector is
        when "00" => mux_data <= adc0_data;
        when "01" => mux_data <= adc1_data;
        when "10" => mux_data <= adc2_data;
        when "11" => mux_data <= adc3_data;
        when others => mux_data <= adc0_data;
     end case;
  end process;

trigger_capture_inst : entity trigger_capture
    generic map(
      c_data_width    => 64
    )
    port map( 
      clk               => gclk,
      rst               => areset, 

      capture_mode      => capture_mode,
      front_condition   => front_condition,

      capture_level     => capture_level,

      trigger_set_up    => start_sync,
      data              => mux_data,
      vector_valid      => open,
      ext_trig          => '0',
      
      l_up              => open,
      l_down            => open,
      
      trigger_start     => trigger_start
    );
    

adc0_deser_clk : entity serdes_1_to_n_clk_ddr_s8_diff 
  generic map (
    S => 8,
    DIFF_TERM => true
  )
  port map (
    clkin_p         => adc0_lclk_p,
    clkin_n         => adc0_lclk_n,
    rxioclkp        => adc0_deser_clk_rxioclkp,
    rxioclkn        => adc0_deser_clkrxioclkn,
    rx_serdesstrobe => adc0_deser_clkrx_serdesstrobe,
    rx_bufg_x1      => adc0_deser_clkrx_bufg_x1
  );


adc0_deser_datain_p <= adc0_fclk_p & adc0_dx_a_p(0) & adc0_dx_b_p(0) & adc0_dx_a_p(1) & adc0_dx_b_p(1) & adc0_dx_a_p(2) & adc0_dx_b_p(2) & adc0_dx_a_p(3) & adc0_dx_b_p(3);
adc0_deser_datain_n <= adc0_fclk_n & adc0_dx_a_n(0) & adc0_dx_b_n(0) & adc0_dx_a_n(1) & adc0_dx_b_n(1) & adc0_dx_a_n(2) & adc0_dx_b_n(2) & adc0_dx_a_n(3) & adc0_dx_b_n(3);

adc0_data_deser : entity serdes_1_to_n_data_ddr_s8_diff
    generic map(
      S             => 8,
      D             => 9, 
      DIFF_TERM     => true
    )
    port map(
      use_phase_detector    => '1',
      datain_p              => adc0_deser_datain_p,
      datain_n              => adc0_deser_datain_n,
      rxioclkp              => adc0_deser_clk_rxioclkp,
      rxioclkn              => adc0_deser_clkrxioclkn,
      rxserdesstrobe        => adc0_deser_clkrx_serdesstrobe,
      reset                 => areset,
      gclk                  => gclk,
      bitslip               => adc0_deser_bitslip,
      debug_in              => "00",
      data_out              => adc0_deser_data_out,
      debug                 => open
    );

  adc0_data     <= adc0_deser_data_out(8 * 8 - 1 downto 0);
  adc0_frame    <= adc0_deser_data_out(9 * 8 - 1 downto 8*8);

adc1_deser_clk : entity serdes_1_to_n_clk_ddr_s8_diff 
  generic map (
    S => 8,
    DIFF_TERM => true
  )
  port map (
    clkin_p         => adc1_lclk_p,
    clkin_n         => adc1_lclk_n,
    rxioclkp        => adc1_deser_clk_rxioclkp,
    rxioclkn        => adc1_deser_clkrxioclkn,
    rx_serdesstrobe => adc1_deser_clkrx_serdesstrobe,
    rx_bufg_x1      => adc1_deser_clkrx_bufg_x1
  );

adc1_deser_datain_p <= adc1_fclk_p & adc1_dx_a_p(0) & adc1_dx_b_p(0) & adc1_dx_a_p(1) & adc1_dx_b_p(1) & adc1_dx_a_p(2) & adc1_dx_b_p(2) & adc1_dx_a_p(3) & adc1_dx_b_p(3);
adc1_deser_datain_n <= adc1_fclk_n & adc1_dx_a_n(0) & adc1_dx_b_n(0) & adc1_dx_a_n(1) & adc1_dx_b_n(1) & adc1_dx_a_n(2) & adc1_dx_b_n(2) & adc1_dx_a_n(3) & adc1_dx_b_n(3);

adc1_data_deser : entity serdes_1_to_n_data_ddr_s8_diff
    generic map(
      S             => 8,
      D             => 9, 
      DIFF_TERM     => true
    )
    port map(
      use_phase_detector    => '1',
      datain_p              => adc1_deser_datain_p,
      datain_n              => adc1_deser_datain_n,
      rxioclkp              => adc1_deser_clk_rxioclkp,
      rxioclkn              => adc1_deser_clkrxioclkn,
      rxserdesstrobe        => adc1_deser_clkrx_serdesstrobe,
      reset                 => areset,
      gclk                  => gclk,
      bitslip               => adc1_deser_bitslip,
      debug_in              => "00",
      data_out              => adc1_deser_data_out,
      debug                 => open
    );
  adc1_data     <= adc1_deser_data_out(8 * 8 - 1 downto 0);
  adc1_frame    <= adc1_deser_data_out(9 * 8 - 1 downto 8*8);

adc2_deser_clk :  entity serdes_1_to_n_clk_ddr_s8_diff 
  generic map (
    S => 8,
    DIFF_TERM => true
  )
  port map (
    clkin_p         => adc2_lclk_p,
    clkin_n         => adc2_lclk_n,
    rxioclkp        => adc2_deser_clk_rxioclkp,
    rxioclkn        => adc2_deser_clkrxioclkn,
    rx_serdesstrobe => adc2_deser_clkrx_serdesstrobe,
    rx_bufg_x1      => adc2_deser_clkrx_bufg_x1
  );

adc2_deser_datain_p <= adc2_fclk_p & adc2_dx_a_p(0) & adc2_dx_b_p(0) & adc2_dx_a_p(1) & adc2_dx_b_p(1) & adc2_dx_a_p(2) & adc2_dx_b_p(2) & adc2_dx_a_p(3) & adc2_dx_b_p(3);
adc2_deser_datain_n <= adc2_fclk_n & adc2_dx_a_n(0) & adc2_dx_b_n(0) & adc2_dx_a_n(1) & adc2_dx_b_n(1) & adc2_dx_a_n(2) & adc2_dx_b_n(2) & adc2_dx_a_n(3) & adc2_dx_b_n(3);

adc2_data_deser : entity serdes_1_to_n_data_ddr_s8_diff
    generic map(
      S             => 8,
      D             => 9, 
      DIFF_TERM     => true
    )
    port map(
      use_phase_detector    => '1',
      datain_p              => adc2_deser_datain_p,
      datain_n              => adc2_deser_datain_n,
      rxioclkp              => adc2_deser_clk_rxioclkp,
      rxioclkn              => adc2_deser_clkrxioclkn,
      rxserdesstrobe        => adc2_deser_clkrx_serdesstrobe,
      reset                 => areset,
      gclk                  => gclk,
      bitslip               => adc2_deser_bitslip,
      debug_in              => "00",
      data_out              => adc2_deser_data_out,
      debug                 => open
    );
  adc2_data     <= adc2_deser_data_out(8 * 8 - 1 downto 0);
  adc2_frame    <= adc2_deser_data_out(9 * 8 - 1 downto 8*8);

adc3_deser_clk : entity serdes_1_to_n_clk_ddr_s8_diff 
  generic map (
    S => 8,
    DIFF_TERM => true
  )
  port map (
    clkin_p         => adc3_lclk_p,
    clkin_n         => adc3_lclk_n,
    rxioclkp        => adc3_deser_clk_rxioclkp,
    rxioclkn        => adc3_deser_clkrxioclkn,
    rx_serdesstrobe => adc3_deser_clkrx_serdesstrobe,
    rx_bufg_x1      => adc3_deser_clkrx_bufg_x1
  );

adc3_deser_datain_p <= adc3_fclk_p & adc3_dx_a_p(0) & adc3_dx_b_p(0) & adc3_dx_a_p(1) & adc3_dx_b_p(1) & adc3_dx_a_p(2) & adc3_dx_b_p(2) & adc3_dx_a_p(3) & adc3_dx_b_p(3);
adc3_deser_datain_n <= adc3_fclk_n & adc3_dx_a_n(0) & adc3_dx_b_n(0) & adc3_dx_a_n(1) & adc3_dx_b_n(1) & adc3_dx_a_n(2) & adc3_dx_b_n(2) & adc3_dx_a_n(3) & adc3_dx_b_n(3);

adc3_data_deser :  entity serdes_1_to_n_data_ddr_s8_diff
    generic map(
      S             => 8,
      D             => 9, 
      DIFF_TERM     => true
    )
    port map(
      use_phase_detector    => '1',
      datain_p              => adc3_deser_datain_p,
      datain_n              => adc3_deser_datain_n,
      rxioclkp              => adc3_deser_clk_rxioclkp,
      rxioclkn              => adc3_deser_clkrxioclkn,
      rxserdesstrobe        => adc3_deser_clkrx_serdesstrobe,
      reset                 => areset,
      gclk                  => gclk,
      bitslip               => adc3_deser_bitslip,
      debug_in              => "00",
      data_out              => adc3_deser_data_out,
      debug                 => open
    );
    
  adc3_data     <= adc3_deser_data_out(8 * 8 - 1 downto 0);
  adc3_frame    <= adc3_deser_data_out(9 * 8 - 1 downto 8*8);


rec0_rst_all <= (rec0_rst or areset or (not adcx_enable(0)));

data_recorder_0_inst : entity data_recorder
    generic map(
      c_max_num_data            => c_max_num_data,
      c_data_width              => 64
    )
    Port map( 
      rst                       => rec0_rst_all,
      clk                       => gclk,

      start                     => trigger_start,
      num_data                  => num_data,
      start_offset              => trig_position(natural(round(log2(real(c_max_num_data))))-1 downto 0),

      s_data                    => adc0_data,
      s_valid                   => adcx_calib_status(0),
      s_ready                   => open,

      m_data                    => rec0_m_data,
      m_valid                   => rec0_m_valid,
      m_ready                   => rec0_m_ready,
      
      compleat                  => open
    );
 
rec1_rst_all <= (rec1_rst or areset or (not adcx_enable(1)));

data_recorder_1_inst : entity data_recorder
    generic map(
      c_max_num_data            => c_max_num_data,
      c_data_width              => 64
    )
    Port map( 
      rst                       => rec1_rst_all,
      clk                       => gclk,

      start                     => trigger_start,
      num_data                  => num_data,
      start_offset              => trig_position(natural(round(log2(real(c_max_num_data))))-1 downto 0),

      s_data                    => adc1_data,
      s_valid                   => adcx_calib_status(1),
      s_ready                   => open,

      m_data                    => rec1_m_data,
      m_valid                   => rec1_m_valid,
      m_ready                   => rec1_m_ready,
      
      compleat                  => open
    );

rec2_rst_all <= (rec2_rst or areset or (not adcx_enable(2)));

data_recorder_2_inst : entity data_recorder
    generic map(
      c_max_num_data            => c_max_num_data,
      c_data_width              => 64
    )
    Port map( 
      rst                       => rec2_rst_all,
      clk                       => gclk,

      start                     => trigger_start,
      num_data                  => num_data,
      start_offset              => trig_position(natural(round(log2(real(c_max_num_data))))-1 downto 0),

      s_data                    => adc2_data,
      s_valid                   => adcx_calib_status(2),
      s_ready                   => open,

      m_data                    => rec2_m_data,
      m_valid                   => rec2_m_valid,
      m_ready                   => rec2_m_ready,
      
      compleat                  => open
    );

rec3_rst_all <= (rec3_rst or areset or (not adcx_enable(3)));

data_recorder_3_inst : entity data_recorder
    generic map(
      c_max_num_data            => c_max_num_data,
      c_data_width              => 64
    )
    Port map( 
      rst                       => rec3_rst_all,
      clk                       => gclk,

      start                     => trigger_start,
      num_data                  => num_data,
      start_offset              => trig_position(natural(round(log2(real(c_max_num_data))))-1 downto 0),

      s_data                    => adc3_data,
      s_valid                   => adcx_calib_status(3),
      s_ready                   => open,

      m_data                    => rec3_m_data,
      m_valid                   => rec3_m_valid,
      m_ready                   => rec3_m_ready,
      
      compleat                  => open
    );


adcx_data_valid <= rec3_m_valid & rec2_m_valid & rec1_m_valid & rec0_m_valid;


spifi_sio <= PCS_O when spifi_T = '0' else (others => 'Z');

PCS_I <= spifi_sio;

spifi_switch_byte_switch_proc :
  process(spifi_sck, spifi_cs)
  begin
    if (spifi_cs = '1') then 
      spifi_T <= '1';
      spifi_cmd_counter <= '0';
    elsif falling_edge(spifi_sck) then
      if (spifi_cmd_valid = '1') then

        if (spifi_cmd_counter = '0') then
          spifi_cmd_counter <= '1';
          spifi_switch_byte <= spifi_cmd_byte;
          spifi_T <= '0'; 
        end if;

      end if;
    end if;
  end process;

--spifi_mux_data_process :
--  process (spifi_switch_byte, adc0_valid, adc1_valid, adc2_valid, adc3_valid, adc0_data, adc1_data, adc2_data, adc3_data)
--  begin
--    --rec0_m_ready <= 0;
--    --rec1_m_ready <= 0;
--    --rec2_m_ready <= 0;
--    --rec3_m_ready <= 0;
--     case spifi_switch_byte is
--        when x"00" => spifi_s_data  <= adc0_data;
--                      spifi_s_valid <= adc0_valid;
--                      --rec0_m_ready <= spifi_s_ready;
--        when x"01" => spifi_s_data  <= adc1_data;
--                      spifi_s_valid <= adc1_valid;
--                      --rec1_m_ready <= spifi_s_ready;
--        when x"02" => spifi_s_data  <= adc2_data;
--                      spifi_s_valid <= adc2_valid;
--                      --rec2_m_ready <= spifi_s_ready;
--        when x"03" => spifi_s_data  <= adc3_data;
--                      spifi_s_valid <= adc3_valid;
--                      --rec3_m_ready <= spifi_s_ready;
--        when others => spifi_s_data  <= adc0_data;
--                       spifi_s_valid <= adc0_valid;
--                      --rec0_m_ready <= spifi_s_ready;
--     end case;
--  end process;

spifi_mux_data_process :
  process (spifi_switch_byte, rec0_m_data, rec1_m_data, rec2_m_data, rec3_m_data, rec0_m_valid, rec1_m_valid, rec2_m_valid, rec3_m_valid, spifi_cs_up, spifi_s_ready_sync)
  begin
    rec0_rst <= '0';
    rec1_rst <= '0';
    rec2_rst <= '0';
    rec3_rst <= '0';
    rec0_m_ready <= '0';
    rec1_m_ready <= '0';
    rec2_m_ready <= '0';
    rec3_m_ready <= '0';
     case spifi_switch_byte is
        when x"00" => spifi_s_data <= rec0_m_data;
                      spifi_s_valid <= rec0_m_valid;
                      rec0_rst <= spifi_cs_up;
                      rec0_m_ready <= spifi_s_ready_sync;
        when x"01" => spifi_s_data <= rec1_m_data;
                      spifi_s_valid <= rec1_m_valid;
                      rec1_rst <= spifi_cs_up;
                      rec1_m_ready <= spifi_s_ready_sync;
        when x"02" => spifi_s_data <= rec2_m_data;
                      spifi_s_valid <= rec2_m_valid;
                      rec2_rst <= spifi_cs_up;
                      rec2_m_ready <= spifi_s_ready_sync;
        when x"03" => spifi_s_data <= rec3_m_data;
                      spifi_s_valid <= rec3_m_valid;
                      rec3_rst <= spifi_cs_up;
                      rec3_m_ready <= spifi_s_ready_sync;
        when others => spifi_s_data <= rec0_m_data;
                       spifi_s_valid <= rec0_m_valid;
                       rec0_rst <= spifi_cs_up;
                       rec0_m_ready <= spifi_s_ready_sync;
     end case;
  end process;

spifi_module_sync_proc : process(spifi_cs, gclk)
begin
  if (spifi_cs = '1') then
    spifi_s_ready_dvec <= (others => '0');
    spifi_s_ready_sync <= '0';
  elsif rising_edge(gclk) then
    spifi_s_ready_dvec(0) <= spifi_s_ready;
    spifi_s_ready_dvec(spifi_s_ready_dvec'length - 1 downto 1) <= spifi_s_ready_dvec(spifi_s_ready_dvec'length - 2 downto 0);
    spifi_s_ready_sync <= ((spifi_s_ready_dvec(spifi_s_ready_dvec'length - 1) and (not spifi_s_ready_dvec(spifi_s_ready_dvec'length - 2))) and (not spifi_T));
  end if;
end process;
 
process(gclk)
begin
  if rising_edge(gclk) then
    spifi_cs_dvec(0) <= spifi_cs;
    spifi_cs_dvec(spifi_cs_dvec'length - 1 downto 1) <= spifi_cs_dvec(spifi_cs_dvec'length - 2 downto 0);
    spifi_cs_up <= (not spifi_s_ready_dvec(spifi_s_ready_dvec'length - 1) and (spifi_s_ready_dvec(spifi_s_ready_dvec'length - 2)));
  end if;
end process;


spifi_module_inst : entity spifi_module
    generic map(
      C_CPHA            => '0',
      C_CPOL            => '0',
      C_LSB_FIRST       => false,
      C_NUM_QBURST      => C_BURST_WIDTH_SPIFI
    )
    port map( 
      SCK               => spifi_sck,
      CS                => spifi_cs,

      PCS_I             => PCS_I,
      PCS_O             => PCS_O,

      s_data            => spifi_s_data,
      s_valid           => spifi_s_valid,
      s_ready           => spifi_s_ready,

      cmd_byte          => spifi_cmd_byte,
      cmd_valid         => spifi_cmd_valid
    );
end Behavioral;
