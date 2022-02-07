----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 11.04.2019 17:38:20
-- Design Name: 
-- Module Name: defPulse - Behavioral
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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
library UNISIM;
use UNISIM.VComponents.all;

entity defPulse is
    Port ( 
      clk            : in std_logic;
      rst            : in std_logic;
      delay          : in std_logic_vector(15 downto 0);
      s_in           : in std_logic;
      s_out          : out std_logic
    );
end defPulse;

architecture Behavioral of defPulse is
    signal state             : std_logic;
    signal cnt               : std_logic_vector(15 downto 0);

begin

process(clk, rst)
begin
  if (rst = '1') then
    s_out <= '0';
    state <= '0';
  elsif rising_edge(clk) then
    case state is
      when '0' => 
        if (s_in = '1') then
          state <= '1';
          cnt <= (others => '0');
        else 
          s_out <= '0';
        end if;
      when '1' =>
        if (cnt < delay) then
          cnt <= cnt + 1;
        else
          s_out <= '1';
          state <= '0';
        end if;
      when others =>
        state <= '0';
    end case;
  end if;
end process;


end Behavioral;
