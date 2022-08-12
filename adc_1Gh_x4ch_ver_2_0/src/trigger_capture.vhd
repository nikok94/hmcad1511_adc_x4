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
    c_data_width      : integer := 64
  );
  Port (
    clk               : in std_logic;
    rst               : in std_logic;

    front             : in std_logic_vector(1 downto 0); -- Задает условия захвата данных: b10 – по нарастающему b01 – по спадающему
    level             : in std_logic_vector(7 downto 0);

    cnt               : in std_logic_vector(32 - 1 downto 0);
    data              : in std_logic_vector(c_data_width - 1 downto 0); -- входные значения данных от АЦП
    valid             : in std_logic;

    trigger           : out std_logic; -- выходной сигнал управляет модулем захвата данных
    cnt_out           : out std_logic_vector(cnt'length - 1 downto 0)
  );
end trigger_capture;

architecture Behavioral of trigger_capture is
    signal level_up_vect    : std_logic_vector(c_data_width/8 - 1 downto 0);
    signal level_down_vect  : std_logic_vector(c_data_width/8 - 1 downto 0);
    signal data_to_compare  : std_logic_vector(c_data_width + 7 downto 0);
    signal old_data_byte    : std_logic_vector(7 downto 0);
    signal trg              : std_logic;
    signal cnt_d            : std_logic_vector(cnt'length - 1 downto 0);
    signal valid_d          : std_logic;

begin

trigger_procces : process(front, level_up_vect, level_down_vect)
begin
  trg <= '0';
  case front is
    when "01" =>
      if (level_down_vect /= x"00") then
        trg <= '1';
      end if;
    when others =>
      if (level_up_vect /= x"00") then
        trg <= '1';
      end if;
  end case;
end process;


old_data_byte_process :
  process(clk)
  begin
    if rising_edge(clk) then
      old_data_byte <= data(7 downto 0);
    end if;
  end process;

  process(clk, rst)
  begin
    if (rst = '1') then
      trigger <= '0';
    elsif rising_edge(clk) then
      if (valid = '1') then
        cnt_d <= cnt;
        valid_d <= valid;
      end if;
      
      if (valid_d = '1') then
        if (trg = '1') then
          cnt_out <= cnt_d;
        end if;
        trigger <= trg;
      else
        trigger <= '0';
      end if;
    end if;
  end process;


data_to_compare   <= old_data_byte & data;

generate_process : for i in 1 to c_data_width/8 generate
level_up_compare_proc:
  process(clk, rst)
  begin
    if (rst = '1') then
      level_up_vect(i - 1) <= '0';
    elsif rising_edge(clk) then
      if (valid = '1') then
        if ((data_to_compare(8*(i - 1) + 7 downto 8*(i - 1)) >= level) and (data_to_compare(8*i + 7 downto 8*i) < data_to_compare(8*(i - 1) + 7 downto 8*(i - 1))))then
          level_up_vect(i - 1) <= '1';
        else
          level_up_vect(i - 1) <= '0';
        end if;
      end if;
    end if;
  end process;

level_down_compare_proc:
  process(clk, rst)
  begin
    if (rst = '1') then
      level_down_vect(i - 1) <= '0';
    elsif rising_edge(clk) then
      if (valid = '1') then
        if ((data_to_compare(8*(i - 1) + 7 downto 8*(i - 1)) <= level) and (data_to_compare(8*i + 7 downto 8*i) > data_to_compare(8*(i - 1) + 7 downto 8*(i - 1))))then
            level_down_vect(i - 1) <= '1';
        else
          level_down_vect(i - 1) <= '0';
        end if;
      end if;
    end if;
  end process;
end generate generate_process;


end Behavioral;