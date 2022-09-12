----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 30.08.2022 16:07:57
-- Design Name: 
-- Module Name: data_recorder_TB - Behavioral
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

library work;
use work.data_recorder;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity data_recorder_TB is
--    Port ( );
end data_recorder_TB;

architecture Behavioral of data_recorder_TB is
  signal rst        : std_logic := '1';
  signal clk        : std_logic := '0';
  constant c_max_num_data   : integer := 256;
  constant c_cnt_width      : integer := 32;
  constant c_data_width     : integer := 64;
  signal mark_delay : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0) := x"0A";
  signal mark_cnt   : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0) := x"0F";
  signal stop       : std_logic;
  signal stop_cnt   : std_logic_vector(c_cnt_width - 1 downto 0);
  signal s_cnt      : std_logic_vector(c_cnt_width - 1 downto 0);
  signal s_data     : std_logic_vector(c_data_width - 1 downto 0);
  signal s_valid    : std_logic;
  signal s_ready    : std_logic;
  signal m_en       : std_logic := '0';
  signal m_data     : std_logic_vector(c_data_width - 1 downto 0);
  signal m_valid    : std_logic;
  signal m_ready    : std_logic := '0';
  type state_type is (IDLE, WAIT_T, CONT, READ_M);
  signal state      : state_type;
  signal state_cnt  : integer := 0;
begin

process
begin
  clk <= '0';
  wait for 5 ns;
  clk <= '1';
  wait for 5 ns;
end process;

rst <= '0' after 100 ns;

process(rst, clk)
begin
  if (rst = '1') then
    state <= IDLE;
    stop <= '0';
    stop_cnt <= (others => '0');
    m_ready <= '0';
    m_en <= '0';
  elsif rising_edge(clk) then
    case (state) is
      when IDLE =>
        if (s_ready = '1') then
          state <= WAIT_T;
        end if;
        state_cnt <= 0;
        stop <= '0';
        m_ready <= '0';
        m_en <= '0';
      when WAIT_T => 
        if (s_valid = '1') then
          if state_cnt < 255 then
            state_cnt <= state_cnt + 1;
          else
            state <= CONT;
            stop <= '1';
            stop_cnt <= s_cnt + 5;
          end if;
        end if;
      when CONT => 
        stop <= '0';
        if (m_valid = '1') then
          state <= READ_M; 
        end if;
      when READ_M =>
        m_ready <= '1';
        m_en <= '1';
      when others => 
        state <= IDLE;
    end case;
  end if;
end process;


process(rst, clk)
begin
  if (rst = '1') then
    s_data <= (others => '0');
    s_cnt <= (others => '0');
    s_valid <= '0';
  elsif rising_edge(clk) then
    if (s_valid = '0') then
      s_valid <= '1';
    else
      s_valid <= '0';
      s_data <= s_data + 1;
      s_cnt <= s_cnt + 1;
    end if;
  end if;
end process;

data_recorder_inst : entity data_recorder
  Generic map(
    c_max_num_data            => c_max_num_data,
    c_data_width              => 64,
    c_cnt_width               => 32
  )
  Port map( 
    rst                       => rst,

    mark_delay                => mark_delay,
    mark_cnt                  => mark_cnt  ,

    stop                      => stop    ,
    stop_cnt                  => stop_cnt,

    s_clk                     => clk,
    s_cnt                     => s_cnt  ,
    s_data                    => s_data ,
    s_valid                   => s_valid,
    s_ready                   => s_ready,

    m_clk                     => clk,
    m_en                      => m_en   ,
    m_data                    => m_data ,
    m_valid                   => m_valid,
    m_ready                   => m_ready

  );


end Behavioral;
