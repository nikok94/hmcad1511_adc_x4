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
      fclk_n                    : in std_logic;
      dx_a_p                    : in std_logic_vector(3 downto 0);
      dx_a_n                    : in std_logic_vector(3 downto 0);
      dx_b_p                    : in std_logic_vector(3 downto 0);
      dx_b_n                    : in std_logic_vector(3 downto 0);
      
      mark_delay                : in std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);
      mark_length               : in std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);
      
      trigger_enable            : in std_logic;
      trigger_condition         : in std_logic_vector(1 downto 0);
      trigger_level             : in std_logic_vector(7 downto 0);
      trigger_mode              : in std_logic_vector(1 downto 0);
      trigger_set               : in std_logic;

      trigger_out               : out std_logic;
      trigger_in                : in std_logic;
      
      areset                    : in std_logic;
      enable                    : in std_logic;
      gclk                      : in std_logic;
      gclk_out                  : out std_logic;

      calib_done                : out std_logic;

      recorder_interrupt        : out std_logic;
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
  signal deser_data_out_rev             : std_logic_vector(9*8 - 1 downto 0);
  signal valid                          : std_logic;
  signal frame                          : std_logic_vector(7 downto 0);
  signal datain_p                       : std_logic_vector(8 downto 0);
  signal datain_n                       : std_logic_vector(8 downto 0);
  signal state                          : integer ;
  signal recorder_rst_all               : std_logic;
  signal data                           : std_logic_vector(63 downto 0);
  signal data_valid                     : std_logic;
  signal recorder_rst_dvec              : std_logic_vector(2 downto 0);
  signal recorder_rdy_dvec              : std_logic_vector(2 downto 0);
  signal recorder_rst_sync              : std_logic;
  signal recorder_rdy_sync              : std_logic;
  signal d0_trigger_set                 : std_logic;
  signal d1_trigger_set                 : std_logic;
  signal d2_trigger_set                 : std_logic;
  signal set                            : std_logic;
  signal trigger_in_dvec                : std_logic_vector(3 downto 0);
  signal recorder_start                 : std_logic;
--  signal offset                         : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);
--  signal offset_d                       : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);
  signal trigger_rst                    : std_logic;
  signal deser_rst                      : std_logic;
  signal bs_counter                     : std_logic_vector(4 downto 0);
  signal tick_counter                   : integer;
  signal rec_valid                      : std_logic;
  signal gclk_bufg                      : std_logic;

begin

recorder_irq_proc :
  process(gclk, recorder_rst_all)
  begin
    if (recorder_rst_all = '1') then
      recorder_interrupt <= '0';
      trigger_rst <= '1';
    elsif rising_edge(gclk) then
      if (rec_valid = '1') then
        recorder_interrupt <= '1';
      end if;
      trigger_rst <= not trigger_enable;
    end if;
  end process;

trigger_sync_process :
  process(gclk)
  begin
    if rising_edge(gclk) then
      d0_trigger_set       <= trigger_set;
      d1_trigger_set       <= d0_trigger_set;
      d2_trigger_set       <= d1_trigger_set;
      
      set <= (d2_trigger_set) and (not d1_trigger_set);
    end if;
  end process;

trigger_capture_inst : entity trigger_capture
  generic map(
    c_data_width    => 64
  )
  port map( 
    clk               => gclk,
    rst               => trigger_rst, 

    capture_mode      => trigger_mode,
    front_condition   => trigger_condition,

    capture_level     => trigger_level,

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
  if (areset = '1') then
    state <= 0 ;
    deser_bitslip <= '0' ;
    bs_counter <= (others => '0');
    valid <= '0';
    deser_rst <= '1';
    tick_counter <= 0;
  elsif rising_edge(gclk) then
    case state is
      when 0 =>
        deser_rst <= '1';
        if (tick_counter > 15) then
          tick_counter <= 0;
          state <= 3;
        else
          tick_counter <= tick_counter + 1;
        end if;
      when 1 =>
        if (frame /= frame_sync_pattern) then
          valid <= '0';
          if (bs_counter(bs_counter'length - 1) = '0') then
            deser_bitslip <= '1' ;
            bs_counter <= bs_counter + 1;
            tick_counter <= 0;
            state <= 2 ;
          else
            deser_rst <= '1';
            tick_counter <= 0;
            state <= 0;
          end if;
        else
          valid <= '1';
        end if;
      when 2 =>
        deser_bitslip <= '0';
        if (tick_counter < 15) then
          tick_counter <= tick_counter + 1 ;
        else
         state <= 1 ;
        end if;
      when 3 =>
        bs_counter <= (others => '0');
        deser_rst <= '0';
        if (tick_counter < 128) then
          tick_counter <= tick_counter + 1;
        else
          state <= 1;
        end if;
      when others =>
        state <= 0;
        valid <= '0';
        deser_rst <= '1';
        tick_counter <= 0;
    end case;
  end if ;
end process;

calib_done <= valid when enable = '1' else '1';

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
    rx_bufg_x1      => gclk_bufg
  );
gclk_out <= gclk_bufg;

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
    reset                 => deser_rst,
    gclk                  => gclk,
    bitslip               => deser_bitslip,
    debug_in              => "00",
    data_out              => deser_data_out,
    debug                 => open--,
--    stat_out              => open
  );

--i_gen : for i in 0 to 8 generate
--  j_gen : for j in 0 to 7 generate
--    deser_data_out_rev(8*i + j) <= deser_data_out(8*i + 7 - j);
--  end generate j_gen;
--end generate i_gen;
deser_data_out_rev <= deser_data_out;

process (gclk_bufg)
begin
  if rising_edge(gclk_bufg) then
    frame <= deser_data_out_rev(9 * 8 - 1 downto 8*8);
    data  <= deser_data_out_rev(8 * 8 - 1 downto 0);
  end if ;
end process;

  data_valid <= valid;

sync_process : process(areset, gclk)
begin
  if (areset = '1') then
    trigger_in_dvec <= (others => '0');
    recorder_start <= '0';
  elsif rising_edge(gclk) then
    trigger_in_dvec(0) <= trigger_in;
    trigger_in_dvec(trigger_in_dvec'length - 1 downto 1) <= trigger_in_dvec(trigger_in_dvec'length - 2 downto 0);
    recorder_start <= (not trigger_in_dvec(trigger_in_dvec'length - 1) and (trigger_in_dvec(trigger_in_dvec'length - 2)));

  end if;
end process;

recorder_rst_all <= (recorder_rst or areset or (not enable));

data_recorder_inst : entity data_recorder
  generic map(
    c_max_num_data            => c_max_num_data,
    c_data_width              => 64
  )
  Port map( 
    rst                       => recorder_rst_all,
    clk                       => gclk,
    
    mark_delay                => mark_delay ,
    mark_length               => mark_length,

    start                     => recorder_start,
    num_data                  => num_data,
    start_offset              => recorder_offset,

    s_data                    => data,
    s_valid                   => data_valid,
    s_ready                   => open,

    m_data                    => recorder_data,
    m_valid                   => rec_valid,
    m_ready                   => recorder_ready,
    
    compleat                  => open
  );
  recorder_valid <= rec_valid;

end Behavioral;
