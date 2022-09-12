----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 09.04.2019 16:15:17
-- Design Name: 
-- Module Name: trigger_capture - Behavioral
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
--library UNISIM;
--use UNISIM.VComponents.all;

entity trigger_capture is
  generic (
    c_data_width      : integer := 64;
    c_cnt_width       : integer := 32
  );
  Port (
    clk               : in std_logic;
    rst               : in std_logic;

    front             : in std_logic_vector(1 downto 0); -- Задает условия захвата данных: b10 – по нарастающему b01 – по спадающему
    level             : in std_logic_vector(7 downto 0);

    cnt               : in std_logic_vector(c_cnt_width - 1 downto 0);
    data              : in std_logic_vector(c_data_width - 1 downto 0); -- входные значения данных от АЦП
    valid             : in std_logic;

    trigger           : out std_logic; -- выходной сигнал управляет модулем захвата данных
    cnt_out           : out std_logic_vector(c_cnt_width - 1 downto 0)
  );
end trigger_capture;

architecture Behavioral of trigger_capture is
    signal level_up_vect    : std_logic_vector(c_data_width/8 - 1 downto 0):=(others => '0');
    signal level_down_vect  : std_logic_vector(c_data_width/8 - 1 downto 0):=(others => '0');
    signal level_vect       : std_logic_vector(c_data_width/8 - 1 downto 0):=(others => '0');
    signal data_to_compare  : std_logic_vector(c_data_width + 7 downto 0);
    signal old_data_byte    : std_logic_vector(7 downto 0);
    signal trg              : std_logic;
    signal cnt_d0           : std_logic_vector(c_cnt_width - 1 downto 0);
    signal cnt_d1           : std_logic_vector(c_cnt_width - 1 downto 0);
    signal cnt_d2           : std_logic_vector(c_cnt_width - 1 downto 0);
begin

trigger_procces : process(clk, rst)
begin
  if (rst = '1') then
    level_vect <= (others => '0');
  elsif rising_edge(clk) then
    case front is
      when "01" =>
        level_vect <= level_down_vect;
      when others =>
        level_vect <= level_up_vect;
    end case;
  end if;
end process;

process(clk, rst)
begin
  if (rst = '1') then
    trigger <= '0';
    cnt_d0 <= (others => '0');
    cnt_out <= (others => '0');
  elsif rising_edge(clk) then
    if (valid = '1') then
      cnt_d0 <= cnt;
      cnt_d1 <= cnt_d0;
    end if;
    cnt_d2 <= cnt_d1;
    if (level_vect /= 0) then
      trigger <= '1';
      cnt_out <= cnt_d2;
    else
      trigger <= '0';
    end if;
  end if;
end process;

old_data_byte_process :
  process(clk, rst)
  begin
    if (rst = '1') then
      old_data_byte <= data(7 downto 0);
      data_to_compare <= old_data_byte & data;
    elsif rising_edge(clk) then
      if (valid = '1') then
        old_data_byte <= data(7 downto 0);
        data_to_compare   <= old_data_byte & data;
      end if;
    end if;
  end process;

generate_process : for i in 0 to c_data_width / 8 - 1 generate
level_up_compare_proc:
  process(clk, rst)
  begin
    if (rst = '1') then
      level_up_vect(i) <= '0';
    elsif rising_edge(clk) then
      if (valid = '1') then
        if ((data_to_compare(8 * i + 7 downto 8 * i) >= level) and (data_to_compare(8 * (i + 1) + 7 downto 8 * (i + 1)) < level)) then
          level_up_vect(i) <= '1';
        else
          level_up_vect(i) <= '0';
        end if;
      end if;
    end if;
  end process;

level_down_compare_proc:
  process(clk, rst)
  begin
    if (rst = '1') then
      level_down_vect(i) <= '0';
    elsif rising_edge(clk) then
      if (valid = '1') then
        if ((data_to_compare(8 * i + 7 downto 8 * i) <= level) and (data_to_compare(8 * (i + 1) + 7 downto 8 * (i + 1)) > level)) then
          level_down_vect(i) <= '1';
        else
          level_down_vect(i) <= '0';
        end if;
      end if;
    end if;
  end process;
end generate generate_process;


end Behavioral;