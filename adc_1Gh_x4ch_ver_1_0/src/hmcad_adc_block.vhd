----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 30.09.2020 09:50:32
-- Design Name: 
-- Module Name: hmcad_adc_block - Behavioral
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
use IEEE.MATH_REAL.ALL;

library work;
use work.serdes_1_to_n_clk_ddr_s8_diff;
use work.serdes_1_to_n_data_ddr_s8_diff;
use work.data_recorder;
use work.trigger_capture;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity hmcad_adc_block is
    Generic (
      c_max_num_data            : integer := 128
    );
    Port (


      lclk_p                    : in std_logic;
      lclk_n                    : in std_logic;
      fclk_p                    : in std_logic;
      fclk_n                    : in std_logic;      trigger_enable            : in std_logic;
      trigger_condition         : in std_logic_vector(1 downto 0);
      trigger_level             : in std_logic_vector(7 downto 0);
      trigger_mode              : in std_logic_vector(1 downto 0);
      trigger_set               : in std_logic;

      trigger_out               : out std_logic;
      trigger_in                : in std_logic;
      dx_a_p                    : in std_logic_vector(3 downto 0);
      dx_a_n                    : in std_logic_vector(3 downto 0);
      dx_b_p                    : in std_logic_vector(3 downto 0);
      dx_b_n                    : in std_logic_vector(3 downto 0);
      areset                    : in std_logic;
      enable                    : in std_logic;
      gclk                      : in std_logic;
      gclk_out                  : out std_logic;
      calib_done                : out std_logic;

      recorder_rst              : in std_logic;
      recorder_data             : out std_logic_vector(63 downto 0);
      recorder_valid            : out std_logic;
      recorder_ready            : in std_logic;
      recorder_offset           : in std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0)
    );
end hmcad_adc_block;

architecture Behavioral of hmcad_adc_block is
  constant num_data                     : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0) := (others => '1');
  constant frame_sync_pattern           : std_logic_vector(7 downto 0) := x"0F";
  signal deser_clkrxioclkp              : std_logic;
  signal deser_clkrxioclkn              : std_logic;
  signal deser_clkrx_serdesstrobe       : std_logic;
  signal deser_bitslip                  : std_logic;
  signal deser_data_out                 : std_logic_vector(9*8 - 1 downto 0);
  signal valid                          : std_logic;
  signal frame                          : std_logic_vector(7 downto 0);
  signal datain_p                       : std_logic_vector(8 downto 0);
  signal datain_n                       : std_logic_vector(8 downto 0);
  signal state                          : std_logic ;
  signal count                          : std_logic_vector(3 downto 0);
  signal recorder_rst_all               : std_logic;
  signal data                           : std_logic_vector(63 downto 0);
  signal data_valid                     : std_logic;
  signal recorder_rst_dvec              : std_logic_vector(3 downto 0);
  signal recorder_rdy_dvec              : std_logic_vector(1 downto 0);
  signal recorder_rst_sync              : std_logic;
  signal recorder_rdy_sync              : std_logic;
  signal trigger_capture_rst            : std_logic;
  signal d0_trigger_condition           : std_logic_vector(1 downto 0);
  signal d0_trigger_level               : std_logic_vector(7 downto 0);
  signal d0_trigger_mode                : std_logic_vector(1 downto 0);
  signal d0_trigger_set                 : std_logic;
  signal d1_trigger_condition           : std_logic_vector(1 downto 0);
  signal d1_trigger_level               : std_logic_vector(7 downto 0);
  signal d1_trigger_mode                : std_logic_vector(1 downto 0);
  signal d1_trigger_set                 : std_logic;
  signal d2_trigger_set                 : std_logic;
  signal set                            : std_logic;
  signal trigger_in_dvec                : std_logic_vector(3 downto 0);
  signal recorder_start                 : std_logic;
  signal offset                         : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);
  signal offset_d                       : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);
  signal trigger_rst                    : std_logic;

begin

trigger_sync_process :
  process(gclk)
  begin
    if rising_edge(gclk) then
      offset <= recorder_offset;
      offset_d <= offset;

      d0_trigger_condition <= trigger_condition;
      d0_trigger_level     <= trigger_level    ;
      d0_trigger_mode      <= trigger_mode     ;
      d0_trigger_set       <= trigger_set      ;

      d1_trigger_condition <= d0_trigger_condition;
      d1_trigger_level     <= d0_trigger_level    ;
      d1_trigger_mode      <= d0_trigger_mode     ;
      d1_trigger_set       <= d0_trigger_set      ;
      
      d2_trigger_set <= d1_trigger_set;
      
      set <= (not d2_trigger_set) and d1_trigger_set;

    end if;
  end process;

trigger_capture_rst <= areset or (not enable);

trigger_rst <= (not trigger_enable) or areset;

trigger_capture_inst : entity trigger_capture
  generic map(
    c_data_width    => 64
  )
  port map( 
    clk               => gclk,
    rst               => trigger_rst, 

    capture_mode      => d1_trigger_mode,
    front_condition   => d1_trigger_condition,

    capture_level     => d1_trigger_level,

    trigger_set_up    => set,
    data              => data,
    vector_valid      => open,
    ext_trig          => '0',
    
    l_up              => open,
    l_down            => open,
    
    trigger_start     => trigger_out
  );

process (gclk, areset)
begin
  if areset = '1' then
    state <= '0' ;
    deser_bitslip <= '0' ;
    count <= "0000" ;
    valid <= '0';
  elsif rising_edge(gclk) then
    if (enable = '1') then
      if state = '0' then
        if (frame /= frame_sync_pattern) then
          deser_bitslip <= '1' ;
          state <= '1' ;
          count <= "0000";
          valid <= '0';
        else 
          valid <= '1';
        end if ;
      elsif (state = '1') then
        deser_bitslip <= '0';
        count <= count + 1 ;
        if count = "1111" then
          state <= '0' ;
         end if ;
       end if ;
     else
       valid <= '1';
     end if;
  end if ;
end process ;

calib_done <= valid;

serdes_1_to_n_clk_ddr_s8_diff_inst : entity serdes_1_to_n_clk_ddr_s8_diff 
  generic map (
    S => 8,
    DIFF_TERM => true
  )
  port map (
    clkin_p         => lclk_p,
    clkin_n         => lclk_n,
    rxioclkp        => deser_clkrxioclkp,
    rxioclkn        => deser_clkrxioclkn,
    rx_serdesstrobe => deser_clkrx_serdesstrobe,
    rx_bufg_x1      => gclk_out
  );

datain_p <= fclk_p & dx_a_p(0) & dx_b_p(0) & dx_a_p(1) & dx_b_p(1) & dx_a_p(2) & dx_b_p(2) & dx_a_p(3) & dx_b_p(3);
datain_n <= fclk_n & dx_a_n(0) & dx_b_n(0) & dx_a_n(1) & dx_b_n(1) & dx_a_n(2) & dx_b_n(2) & dx_a_n(3) & dx_b_n(3);

serdes_1_to_n_data_ddr_s8_diff_inst : entity serdes_1_to_n_data_ddr_s8_diff
  generic map(
    S             => 8,
    D             => 9, 
    DIFF_TERM     => true
  )
  port map(
    use_phase_detector    => '1',
    datain_p              => datain_p,
    datain_n              => datain_n,
    rxioclkp              => deser_clkrxioclkp,
    rxioclkn              => deser_clkrxioclkn,
    rxserdesstrobe        => deser_clkrx_serdesstrobe,
    reset                 => areset,
    gclk                  => gclk,
    bitslip               => deser_bitslip,
    debug_in              => "00",
    data_out              => deser_data_out,
    debug                 => open
  );

  frame    <= deser_data_out(9 * 8 - 1 downto 8*8);
process (gclk)
begin
  if rising_edge(gclk) then
    data <= deser_data_out(8 * 8 - 1 downto 0);
    data_valid <= valid;
  end if ;
end process;

sync_process : process(areset, gclk)
begin
  if (areset = '1') then
    recorder_rdy_dvec <= (others => '0');
    recorder_rst_dvec <= (others => '0');
    recorder_rdy_sync <= '0';
    recorder_rst_sync <= '1';
  elsif rising_edge(gclk) then
    trigger_in_dvec(0) <= trigger_in;
    trigger_in_dvec(trigger_in_dvec'length - 1 downto 1) <= trigger_in_dvec(trigger_in_dvec'length - 2 downto 0);
    recorder_start <= (not trigger_in_dvec(trigger_in_dvec'length - 1) and (trigger_in_dvec(trigger_in_dvec'length - 2)));
    
    recorder_rdy_dvec(0) <= recorder_ready;
    recorder_rdy_dvec(recorder_rdy_dvec'length - 1 downto 1) <= recorder_rdy_dvec(recorder_rdy_dvec'length - 2 downto 0);
    recorder_rdy_sync <= (recorder_rdy_dvec(recorder_rdy_dvec'length - 1) and (not recorder_rdy_dvec(recorder_rdy_dvec'length - 2)));
  
    recorder_rst_dvec(0) <= recorder_rst;
    recorder_rst_dvec(recorder_rst_dvec'length - 1 downto 1) <= recorder_rst_dvec(recorder_rst_dvec'length - 2 downto 0);
    recorder_rst_sync <= (not recorder_rst_dvec(recorder_rst_dvec'length - 1) and (recorder_rst_dvec(recorder_rst_dvec'length - 2)));
  end if;
end process;

recorder_rst_all <= (recorder_rst_sync or areset or (not enable));


data_recorder_inst : entity data_recorder
  generic map(
    c_max_num_data            => c_max_num_data,
    c_data_width              => 64
  )
  Port map( 
    rst                       => recorder_rst_all,
    clk                       => gclk,

    start                     => recorder_start,
    num_data                  => num_data,
    start_offset              => offset_d,

    s_data                    => data,
    s_valid                   => data_valid,
    s_ready                   => open,

    m_data                    => recorder_data,
    m_valid                   => recorder_valid,
    m_ready                   => recorder_ready,
    
    compleat                  => open
  );

end Behavioral;
