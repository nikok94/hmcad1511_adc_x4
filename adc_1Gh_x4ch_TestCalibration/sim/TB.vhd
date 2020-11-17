----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 06.10.2020 09:02:35
-- Design Name: 
-- Module Name: TB - Behavioral
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
use work.hmcad_x4_block;
use work.QSPI_interconnect;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity TB is
    --Port ( );
end TB;

architecture Behavioral of TB is

    constant c_max_num_data         : integer := 128;
    signal areset                   : std_logic := '1';
    signal TriggerSetUp             : std_logic_vector(15 downto 0);
    signal ADCEnableReg             : std_logic_vector(15 downto 0):= x"0015";
    signal TriggerPositionSetUp     : std_logic_vector(15 downto 0);
    signal mode                     : std_logic_vector(1 downto 0);
    signal start                    : std_logic;

    signal adc0_lclk_p              : std_logic;
    signal adc0_lclk_n              : std_logic;
    signal adc0_fclk_p              : std_logic;
    signal adc0_fclk_n              : std_logic;
    signal adc0_dx_a_p              : std_logic_vector(3 downto 0);
    signal adc0_dx_a_n              : std_logic_vector(3 downto 0);
    signal adc0_dx_b_p              : std_logic_vector(3 downto 0);
    signal adc0_dx_b_n              : std_logic_vector(3 downto 0);

    signal adc1_lclk_p              : std_logic;
    signal adc1_lclk_n              : std_logic;
    signal adc1_fclk_p              : std_logic;
    signal adc1_fclk_n              : std_logic;
    signal adc1_dx_a_p              : std_logic_vector(3 downto 0);
    signal adc1_dx_a_n              : std_logic_vector(3 downto 0);
    signal adc1_dx_b_p              : std_logic_vector(3 downto 0);
    signal adc1_dx_b_n              : std_logic_vector(3 downto 0);

    signal adc2_lclk_p              : std_logic;
    signal adc2_lclk_n              : std_logic;
    signal adc2_fclk_p              : std_logic;
    signal adc2_fclk_n              : std_logic;
    signal adc2_dx_a_p              : std_logic_vector(3 downto 0);
    signal adc2_dx_a_n              : std_logic_vector(3 downto 0);
    signal adc2_dx_b_p              : std_logic_vector(3 downto 0);
    signal adc2_dx_b_n              : std_logic_vector(3 downto 0);

    signal adc3_lclk_p              : std_logic;
    signal adc3_lclk_n              : std_logic;
    signal adc3_fclk_p              : std_logic;
    signal adc3_fclk_n              : std_logic;
    signal adc3_dx_a_p              : std_logic_vector(3 downto 0);
    signal adc3_dx_a_n              : std_logic_vector(3 downto 0);
    signal adc3_dx_b_p              : std_logic_vector(3 downto 0);
    signal adc3_dx_b_n              : std_logic_vector(3 downto 0);

    signal adcx_calib_done          : std_logic_vector(3 downto 0);
    signal adcx_interrupt           : std_logic_vector(3 downto 0);
    signal adcx_tick_ms             : std_logic_vector(3 downto 0);
    
    signal spifi_cs                 : std_logic;
    signal spifi_sck                : std_logic;
    signal spifi_sio                : std_logic_vector(3 downto 0);
    
    signal frame                    : std_logic_vector(7 downto 0):= "00001111";
    signal d0                       : std_logic_vector(7 downto 0):= x"01";
    signal d1                       : std_logic_vector(7 downto 0):= x"02";
    signal d2                       : std_logic_vector(7 downto 0):= x"03";
    signal d3                       : std_logic_vector(7 downto 0):= x"04";
    signal d4                       : std_logic_vector(7 downto 0):= x"05";
    signal d5                       : std_logic_vector(7 downto 0):= x"06";
    signal d6                       : std_logic_vector(7 downto 0):= x"07";
    signal d7                       : std_logic_vector(7 downto 0):= x"08";
    signal d8                       : std_logic_vector(7 downto 0):= x"09";
    
    signal clk_1000                 : std_logic;
    signal slave_x_clk              : std_logic_vector(4 - 1 downto 0);
    signal slave_x_ready            : std_logic_vector(4 - 1 downto 0);
    signal slave_x_data             : std_logic_vector(4*64 - 1 downto 0);
    signal slave_x_cs_up            : std_logic_vector(4 - 1 downto 0);
    signal qspi_sio                 : std_logic_vector(3 downto 0);
    signal qspi_sck                 : std_logic;
    signal qspi_cs                  : std_logic:= '1';
    signal clk_125                  : std_logic;
    signal sck                      : std_logic;
    signal sck_with_rst             : std_logic;
    signal qspi_reg                 : std_logic_vector(7 downto 0):=  x"01";
    signal qspi_counter             : integer;
    signal state                    : integer := 0;
    signal data                     : std_logic_vector(63 downto 0):= x"5522336655441122";
begin
clk_gen_proc :
  process
  begin
    clk_1000 <= '0';
    wait for 1 ns /2;
    clk_1000 <= '1';
    wait for 1 ns /2;
  end process;

areset <= '0' after 100 ns;

process(clk_1000)
begin
  if rising_edge(clk_1000) then
    frame(frame'length - 1 downto 1) <= frame(frame'length - 2 downto 0);
    frame(0) <= frame(frame'length - 1);
    
    d0(d0'length - 1 downto 1) <= d0(d0'length - 2 downto 0);
    d0(0) <= d0(d0'length - 1);
    
    d1(d1'length - 1 downto 1) <= d1(d1'length - 2 downto 0);
    d1(0) <= d1(d1'length - 1);
    
    d2(d2'length - 1 downto 1) <= d2(d2'length - 2 downto 0);
    d2(0) <= d2(d2'length - 1);
    
    d3(d3'length - 1 downto 1) <= d3(d3'length - 2 downto 0);
    d3(0) <= d3(d3'length - 1);
    
    d4(d4'length - 1 downto 1) <= d4(d4'length - 2 downto 0);
    d4(0) <= d4(d4'length - 1);
    
    d5(d5'length - 1 downto 1) <= d5(d5'length - 2 downto 0);
    d5(0) <= d5(d5'length - 1);
    
    d6(d6'length - 1 downto 1) <= d6(d6'length - 2 downto 0);
    d6(0) <= d6(d6'length - 1);
    
    d7(d7'length - 1 downto 1) <= d7(d7'length - 2 downto 0);
    d7(0) <= d7(d7'length - 1);
  end if;
end process;

adc0_dx_a_p <= d6(0) & d4(0) & d2(0) & d0(0);
adc0_dx_a_n <= (not d6(0)) & (not d4(0)) & (not d2(0)) & (not d0(0));
adc0_dx_b_p <= d7(0) & d5(0) & d3(0) & d1(0);
adc0_dx_b_n <= (not d7(0)) & (not d5(0)) & (not d3(0)) & (not d1(0));

adc1_dx_a_p <= d6(0) & d4(0) & d2(0) & d0(0);
adc1_dx_a_n <= (not d6(0)) & (not d4(0)) & (not d2(0)) & (not d0(0));
adc1_dx_b_p <= d7(0) & d5(0) & d3(0) & d1(0);
adc1_dx_b_n <= (not d7(0)) & (not d5(0)) & (not d3(0)) & (not d1(0));

adc2_dx_a_p <= d6(0) & d4(0) & d2(0) & d0(0);
adc2_dx_a_n <= (not d6(0)) & (not d4(0)) & (not d2(0)) & (not d0(0));
adc2_dx_b_p <= d7(0) & d5(0) & d3(0) & d1(0);
adc2_dx_b_n <= (not d7(0)) & (not d5(0)) & (not d3(0)) & (not d1(0));

adc3_dx_a_p <= d6(0) & d4(0) & d2(0) & d0(0);
adc3_dx_a_n <= (not d6(0)) & (not d4(0)) & (not d2(0)) & (not d0(0));
adc3_dx_b_p <= d7(0) & d5(0) & d3(0) & d1(0);
adc3_dx_b_n <= (not d7(0)) & (not d5(0)) & (not d3(0)) & (not d1(0));

adc0_fclk_p <= frame(0);
adc0_fclk_n <= not frame(0);

adc1_fclk_p <= frame(0);
adc1_fclk_n <= not frame(0);

adc2_fclk_p <= frame(0);
adc2_fclk_n <= not frame(0);

adc3_fclk_p <= frame(0);
adc3_fclk_n <= not frame(0);

adc0_lclk_p_n_gen_proc : 
  process
  begin
    adc0_lclk_p <= '0';
    adc0_lclk_n <= '1';
    wait for 1 ns;
    adc0_lclk_p <= '1';
    adc0_lclk_n <= '0';
    wait for 1 ns;
  end process;

adc1_lclk_p_n_gen_proc : 
  process
  begin
    adc1_lclk_p <= '0';
    adc1_lclk_n <= '1';
    wait for 1 ns;
    adc1_lclk_p <= '1';
    adc1_lclk_n <= '0';
    wait for 1 ns;
  end process;

adc2_lclk_p_n_gen_proc : 
  process
  begin
    adc2_lclk_p <= '0';
    adc2_lclk_n <= '1';
    wait for 1 ns;
    adc2_lclk_p <= '1';
    adc2_lclk_n <= '0';
    wait for 1 ns;
  end process;

adc3_lclk_p_n_gen_proc : 
  process
  begin
    adc3_lclk_p <= '0';
    adc3_lclk_n <= '1';
    wait for 1 ns;
    adc3_lclk_p <= '1';
    adc3_lclk_n <= '0';
    wait for 1 ns;
  end process;

--adc0_fclk_p_n_gen_proc : 
--  process
--  begin
--    adc0_fclk_p <= '0';
--    adc0_fclk_n <= '1';
--    wait for 4 ns;
--    adc0_fclk_p <= '1';
--    adc0_fclk_n <= '0';
--    wait for 4 ns;
--  end process;
--
--adc1_fclk_p_n_gen_proc : 
--  process
--  begin
--    adc1_fclk_p <= '0';
--    adc1_fclk_n <= '1';
--    wait for 4 ns;
--    adc1_fclk_p <= '1';
--    adc1_fclk_n <= '0';
--    wait for 4 ns;
--  end process;
--
--adc2_fclk_p_n_gen_proc : 
--  process
--  begin
--    adc2_fclk_p <= '0';
--    adc2_fclk_n <= '1';
--    wait for 4 ns;
--    adc2_fclk_p <= '1';
--    adc2_fclk_n <= '0';
--    wait for 4 ns;
--  end process;
--
--adc3_fclk_p_n_gen_proc : 
--  process
--  begin
--    adc3_fclk_p <= '0';
--    adc3_fclk_n <= '1';
--    wait for 4 ns;
--    adc3_fclk_p <= '1';
--    adc3_fclk_n <= '0';
--    wait for 4 ns;
--  end process;

--hmcad_x4_block_inst : entity hmcad_x4_block
--  generic map(
--    c_max_num_data          => c_max_num_data
--  )
--  Port map(
--    areset                  => areset              ,
--    TriggerSetUp            => TriggerSetUp        ,
--    ADCEnableReg            => ADCEnableReg        ,
--    TriggerPositionSetUp    => TriggerPositionSetUp,
--    mode                    => mode                ,
--    start                   => start               ,
--
--    adc0_lclk_p             => adc0_lclk_p,
--    adc0_lclk_n             => adc0_lclk_n,
--    adc0_fclk_p             => adc0_fclk_p,
--    adc0_fclk_n             => adc0_fclk_n,
--    adc0_dx_a_p             => adc0_dx_a_p,
--    adc0_dx_a_n             => adc0_dx_a_n,
--    adc0_dx_b_p             => adc0_dx_b_p,
--    adc0_dx_b_n             => adc0_dx_b_n,
--
--    adc1_lclk_p             => adc1_lclk_p,
--    adc1_lclk_n             => adc1_lclk_n,
--    adc1_fclk_p             => adc1_fclk_p,
--    adc1_fclk_n             => adc1_fclk_n,
--    adc1_dx_a_p             => adc1_dx_a_p,
--    adc1_dx_a_n             => adc1_dx_a_n,
--    adc1_dx_b_p             => adc1_dx_b_p,
--    adc1_dx_b_n             => adc1_dx_b_n,
--
--    adc2_lclk_p             => adc2_lclk_p,
--    adc2_lclk_n             => adc2_lclk_n,
--    adc2_fclk_p             => adc2_fclk_p,
--    adc2_fclk_n             => adc2_fclk_n,
--    adc2_dx_a_p             => adc2_dx_a_p,
--    adc2_dx_a_n             => adc2_dx_a_n,
--    adc2_dx_b_p             => adc2_dx_b_p,
--    adc2_dx_b_n             => adc2_dx_b_n,
--
--    adc3_lclk_p             => adc3_lclk_p,
--    adc3_lclk_n             => adc3_lclk_n,
--    adc3_fclk_p             => adc3_fclk_p,
--    adc3_fclk_n             => adc3_fclk_n,
--    adc3_dx_a_p             => adc3_dx_a_p,
--    adc3_dx_a_n             => adc3_dx_a_n,
--    adc3_dx_b_p             => adc3_dx_b_p,
--    adc3_dx_b_n             => adc3_dx_b_n,
--
--    adcx_calib_done         => adcx_calib_done,
--    adcx_interrupt          => adcx_interrupt,
--    adcx_tick_ms            => adcx_tick_ms
--    );


qspi_cs <= '0' after 200 ns;

clk_125_gen :
  process
  begin
    clk_125 <= '0';
    wait for 8 ns / 2;
    clk_125 <= '1';
    wait for 8 ns / 2;
  end process;

sck_gen :
  process
  begin
    sck <= '0';
    wait for 20 ns / 2;
    sck <= '1';
    wait for 20 ns / 2;
  end process;

sck_with_rst <= sck when qspi_cs = '0' else '0';
qspi_sck <= sck_with_rst after 3 ns;

  process(qspi_sck, qspi_cs)
  begin
    if (qspi_cs = '1') then
      state <= 0;
      qspi_counter <= 0;
    elsif falling_edge(qspi_sck) then
      case (state) is
        when 0 =>
          if qspi_counter < 8 then
            qspi_counter <= qspi_counter + 1;
            qspi_reg(qspi_reg'length - 1 downto 1) <= qspi_reg(qspi_reg'length - 2 downto 0);
            qspi_reg(0) <= 'Z';
          else 
            state <= 1;
          end if;
        when 1 =>
          qspi_reg <= (others => 'Z');

        when others =>
        
      end case;
    end if;
  end process;

qspi_sio(0) <= qspi_reg(qspi_reg'length - 1);
qspi_sio(3 downto 1) <= "ZZZ";
slave_x_clk <= clk_125 & clk_125 & clk_125 & clk_125;

process(clk_125)
begin
  if rising_edge(clk_125) then
    if (slave_x_ready /= 0) then
      data <= data + 1;
    end if;
  end if;
end process;

slave_x_data <= data & data & data & data;

QSPI_interconnect_inst : entity QSPI_interconnect
  Generic map(
    c_num_slave_port    => 4,
    c_data_width        => 64,
    c_command_width     => 8,
    C_CPHA              => '0',
    C_CPOL              => '0',
    C_LSB_FIRST         => true
  )
  Port map(
    slave_x_clk         => slave_x_clk  ,
    slave_x_ready       => slave_x_ready,
    slave_x_data        => slave_x_data ,
    slave_x_cs_up       => slave_x_cs_up,
    qspi_sio            => qspi_sio     ,
    qspi_sck            => qspi_sck     ,
    qspi_cs             => qspi_cs      
  );

end Behavioral;
