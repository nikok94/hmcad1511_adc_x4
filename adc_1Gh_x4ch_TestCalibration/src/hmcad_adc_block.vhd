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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
library UNISIM;
use UNISIM.VComponents.all;

entity hmcad_adc_block is
  Port (
    areset                    : in std_logic;
    clk                       : in std_logic;

    lclk_p                    : in std_logic;
    lclk_n                    : in std_logic;
    fclk_p                    : in std_logic;
    fclk_n                    : in std_logic;
    dx_a_p                    : in std_logic_vector(3 downto 0);
    dx_a_n                    : in std_logic_vector(3 downto 0);
    dx_b_p                    : in std_logic_vector(3 downto 0);
    dx_b_n                    : in std_logic_vector(3 downto 0);

    fclk_ibufgds              : out std_logic;
    gclk_out                  : out std_logic;
    gclkdiv2_out              : out std_logic;
    gclkdiv4_out              : out std_logic;
    d_bs                      : in std_logic;

    data                      : out std_logic_vector(9*8-1 downto 0)--;
--    data_valid                : out std_logic

  );
end hmcad_adc_block;

architecture Behavioral of hmcad_adc_block is
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
  signal deser_rst                      : std_logic;
  signal bs_counter                     : std_logic_vector(3 downto 0);
  signal tick_counter                   : integer;
  signal gclkdiv2                       : std_logic := '0';
  signal gclkdiv4                       : std_logic := '0';
  signal gclk_bufg                      : std_logic;
  signal data_ibufgds                   : std_logic_vector(8 downto 0);

begin

tick_ms_proc :
  process(gclk_bufg)
  begin
    if rising_edge(gclk_bufg) then
      if gclkdiv2 = '0' then
        gclkdiv4 <= not gclkdiv4;
      end if;
      gclkdiv2 <= not gclkdiv2;
      gclkdiv4_out <= gclkdiv4;
      gclkdiv2_out <= gclkdiv2;
    end if;
  end process;

--process (gclk_bufg, areset)
--begin
--  if (areset = '1') then
--    state <= 0 ;
--    deser_bitslip <= '0' ;
--    bs_counter <= (others => '0');
--    valid <= '0';
--    deser_rst <= '1';
--    tick_counter <= 0;
--  elsif rising_edge(gclk_bufg) then
--    case state is
--      when 0 =>
--        bs_counter <= (others => '0');
--        deser_rst <= '0';
--        if (tick_counter < 125) then
--          tick_counter <= tick_counter + 1;
--        else
--          state <= 1;
--        end if;
--      when 1 =>
--        if (frame /= frame_sync_pattern) then
--          valid <= '0';
--          if (bs_counter(bs_counter'length - 1) = '0') then
--            deser_bitslip <= '1' ;
--            bs_counter <= bs_counter + 1;
--            tick_counter <= 0;
--            state <= 2 ;
--          else
--            deser_rst <= '1';
--            tick_counter <= 0;
--            state <= 0;
--          end if;
--        else
--          valid <= '1';
--        end if;
--      when 2 =>
--        deser_bitslip <= '0';
--        tick_counter <= tick_counter + 1 ;
--        if (tick_counter > 125000) then
--         state <= 1 ;
--        end if;
--      when others =>
--        state <= 0;
--        valid <= '0';
--        deser_rst <= '1';
--        tick_counter <= 0;
--    end case;
--  end if ;
--end process;

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
    reset                 => areset,
    gclk                  => clk,
    bitslip               => d_bs,
    debug_in              => "00",
    data_out              => deser_data_out,
    data_ibufgds          => data_ibufgds,
    debug                 => open
  );

bufg_fclk : bufg 
  port map (
    I => data_ibufgds(8),
    O => fclk_ibufgds
  );

--fclk_ibufgds <= data_ibufgds(8);
--
----i_gen : for i in 0 to 8 generate
----  j_gen : for j in 0 to 7 generate
----    deser_data_out_rev(8*i + j) <= deser_data_out(8*i + 7 - j);
----  end generate j_gen;
----end generate i_gen;

deser_data_out_rev <= deser_data_out;

process (clk, areset)
begin
  if (areset = '1') then
--    data_valid <= '0';
    data <= (others => '0');
  elsif rising_edge(clk) then
--    frame <= deser_data_out_rev(9 * 8 - 1 downto 8*8);
    data  <= deser_data_out_rev(9 * 8 - 1 downto 0);
--    data_valid <= valid;
  end if ;
end process;

end Behavioral;
