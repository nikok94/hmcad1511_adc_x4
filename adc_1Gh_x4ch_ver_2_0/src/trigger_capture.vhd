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
        c_data_width    : integer := 64
    );
    Port ( 
      clk               : in std_logic;
      rst               : in std_logic;

      capture_mode      : in std_logic_vector(1 downto 0); -- Задает условия запуска b01 – старт по уровню (нормальный) b10 – auto b11 –внешний 
      front_condition   : in std_logic_vector(1 downto 0); -- Задает условия захвата данных: b10 – по нарастающему b01 – по спадающему

      capture_level     : in std_logic_vector(7 downto 0);

      trigger_set_up    : in std_logic;
      data              : in std_logic_vector(c_data_width-1 downto 0); -- входные значения данных от АЦП
      vector_valid      : out std_logic_vector(c_data_width/8 - 1 downto 0);
      ext_trig          : in std_logic; -- внешний триггер
      
      l_up              : out std_logic_vector(c_data_width/8 - 1 downto 0);
      l_down            : out std_logic_vector(c_data_width/8 - 1 downto 0);
      
      trigger_start     : out std_logic -- выходной сигнал управляет модулем захвата данных
    );
end trigger_capture;

architecture Behavioral of trigger_capture is
    signal level_up_vect    : std_logic_vector(c_data_width/8 - 1 downto 0);
    signal level_down_vect  : std_logic_vector(c_data_width/8 - 1 downto 0);
    signal data_to_compare  : std_logic_vector(c_data_width + 7 downto 0);
    signal old_data_byte    : std_logic_vector(7 downto 0);
    type state_machine      is (idle, start_ready, level_up_state, level_down_state, ext_trig_state, trigger_start_state, trigger_wait_state, trigger_restart_state, double_state);
    signal state, next_state: state_machine;
    signal vector_o         : std_logic_vector(c_data_width/8 - 1 downto 0);
    signal start            : std_logic;
    signal counter          : std_logic_vector(7 downto 0);
    signal double           : std_logic;
    signal next_double      : std_logic;

begin

process(clk, rst)
begin
  if (rst = '1') then
    vector_o <= (others => '0');
  elsif rising_edge(clk) then
     case state is 
       when level_up_state => 
         if (level_up_vect /= 0) then
          vector_o <= level_up_vect;
        end if;
       when level_down_state => 
          if (level_down_vect /= 0) then
          vector_o <= level_down_vect;
        end if;
       when others => 
     end case;
  end if;
end process;

vector_valid <= vector_o;

sync_process:
process(clk, rst)
begin
  if (rst = '1') then
    state <= idle;
    double <='0';
    trigger_start <= '0';
    counter <= (others => '0');
  elsif rising_edge(clk) then
    state <= next_state;
    trigger_start <= start;
    if ((state = trigger_start_state) or (state = trigger_wait_state)) then
      counter <= counter + 1;
    else
      counter <= (others => '0');
    end if;
    double <= next_double;
  end if;
end process;

next_state_process:
process(state, trigger_set_up, capture_mode, level_up_vect, ext_trig, front_condition, counter, double)
begin
  next_state <= state;
  start <= '0';
    case state is
      when idle =>
        next_state <= start_ready;
        next_double <= '0';
      when start_ready =>
        if (trigger_set_up = '1') then
          next_state <= trigger_start_state;
        end if;
      when trigger_restart_state =>
        next_double <= '1';
        case capture_mode is 
          when "10" => 
            next_state <= trigger_start_state;
          when "01" =>
            if (front_condition(0) = '1') then
              next_state <= level_down_state;
            elsif (front_condition(1) = '1') then
              next_state <= level_up_state;
            else
              next_state <= idle;
            end if;
          when "11" => 
            next_state <= ext_trig_state;
          when others => 
            next_state <= idle;
        end case;
      when double_state =>
        if (double = '1') then
          next_state <= idle;
        else
          next_state <= trigger_wait_state;
        end if;
      when trigger_wait_state => 
        if (counter = x"0F") then
          next_state <= trigger_restart_state;
        end if;
      when level_up_state =>
        if (level_up_vect /= 0) then
          next_state <= trigger_start_state;
        end if;
      when level_down_state => 
        if (level_down_vect /= 0) then
          next_state <= trigger_start_state;
        end if;
      when ext_trig_state => 
        if (ext_trig = '1') then
          next_state <= trigger_start_state;
        end if;
      when trigger_start_state => 
        start <= '1';
        if (counter = x"08") then
          next_state <= double_state;
        end if;
      when others => 
        next_state <= idle;
    end case;
end process;

l_up <= level_up_vect;
l_down <= level_down_vect;
  
old_data_byte_process :
  process(clk)
  begin
    if rising_edge(clk) then
      old_data_byte <= data(7 downto 0);
      data_to_compare   <= old_data_byte & data;
    end if;
  end process;

generate_process : for i in 1 to c_data_width/8 generate
level_up_compare_proc:
  process(clk)
  begin
    if rising_edge(clk) then
      if ((data_to_compare(8*(i - 1) + 7 downto 8*(i - 1)) >= capture_level) and (data_to_compare(8*i + 7 downto 8*i) < data_to_compare(8*(i - 1) + 7 downto 8*(i - 1))))then
        level_up_vect(i - 1) <= '1';
      else
        level_up_vect(i - 1) <= '0';
      end if;
    end if;
  end process;

level_down_compare_proc:
  process(clk)
  begin
    if rising_edge(clk) then
      if ((data_to_compare(8*(i - 1) + 7 downto 8*(i - 1)) <= capture_level) and (data_to_compare(8*i + 7 downto 8*i) > data_to_compare(8*(i - 1) + 7 downto 8*(i - 1))))then
          level_down_vect(i - 1) <= '1';
      else
        level_down_vect(i - 1) <= '0';
      end if;
    end if;
  end process;
end generate generate_process;


end Behavioral;