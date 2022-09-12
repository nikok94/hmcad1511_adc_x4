----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 30.08.2022 10:07:37
-- Design Name: 
-- Module Name: trigger_capture_TB - Behavioral
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

library work;
use work.trigger_capture;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity trigger_capture_TB is
--    Port ( );
end trigger_capture_TB;

architecture Behavioral of trigger_capture_TB is
  signal clk_100MHz         : std_logic;
  signal rst                : std_logic:= '1';
  signal cnt                : std_logic_vector(31 downto 0);
  signal data               : std_logic_vector(64 - 1 downto 0); -- входные значения данных от АЦП
  signal valid              : std_logic;
  signal trigger            : std_logic; -- выходной сигнал управляет модулем захвата данных
  signal cnt_out            : std_logic_vector(31 downto 0);
  signal dbyte              : std_logic_vector(7 downto 0);
  signal front              : std_logic_vector(1 downto 0):= "01"; -- Задает условия захвата данных: b10 – по нарастающему b01 – по спадающему
  signal level              : std_logic_vector(7 downto 0):= x"05";

begin

process
begin
  clk_100MHz <= '0';
  wait for 5 ns;
  clk_100MHz <= '1';
  wait for 5 ns;
end process;

rst <= '0' after 100 ns;

process(clk_100MHz, rst)
variable vparity        : std_logic_vector(64 - 1 downto 0);
begin
  if (rst = '1') then
    valid <= '0';
    dbyte <= (others => '0');
    data <= (others => '0');
    cnt <= (others => '0');
  elsif rising_edge(clk_100MHz) then
    if (valid = '1') then
      valid <= '0';
      cnt <= cnt + 1;
    else
      valid <= '1';
      dbyte <= dbyte + 8;
      l_parity : for i in 0 to 7 loop
        vparity(i * 8 + 7 downto i * 8 + 0) := dbyte + 7 - i;
      end loop l_parity;
      data <= vparity;
    end if;
  end if;
end process;




trigger_capture_inst: entity trigger_capture
  Generic map(
    c_data_width      => 64
  )
  Port map(
    clk               => clk_100MHz,
    rst               => rst,

    front             => front, -- Задает условия захвата данных: b10 – по нарастающему b01 – по спадающему
    level             => level,

    cnt               => cnt    ,
    data              => data   ,
    valid             => valid  ,

    trigger           => trigger,
    cnt_out           => cnt_out
  );

end Behavioral;
