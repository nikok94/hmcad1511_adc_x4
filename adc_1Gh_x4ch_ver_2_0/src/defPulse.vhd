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
      s_out_p        : out std_logic;
      s_out_n        : out std_logic
    );
end defPulse;

architecture Behavioral of defPulse is
    signal state             : std_logic_vector(1 downto 0);
    signal cnt               : std_logic_vector(15 downto 0);
    signal s_in_dvec         : std_logic_vector(2 downto 0);
    signal ss                : std_logic;

begin

process(clk, rst)
begin
  if (rst = '1') then
    s_out_p <= '0';
    s_out_n <= '0';
    state <= "00";
  elsif rising_edge(clk) then
    s_in_dvec(0) <= s_in;
    s_in_dvec(s_in_dvec'length-1 downto 1) <= s_in_dvec(s_in_dvec'length-2 downto 0);
    ss <= (not s_in_dvec(s_in_dvec'length-1)) and s_in_dvec(s_in_dvec'length-2);
    case state is
      when "00" => 
        if (ss = '1') then
          state <= "01";
          cnt <= (others => '0');
        else 
          s_out_n <= '0';
          s_out_p <= '0';
        end if;
      when "01" =>
        if (cnt < delay) then
          cnt <= cnt + 1;
        else
          s_out_p <= '1';
          s_out_n <= '0';
          state <= "10";
        end if;
      when "10" =>
          s_out_p <= '0';
          s_out_n <= '1';
          state <= "00";
      when others =>
        state <= "00";
    end case;
  end if;
end process;


end Behavioral;
