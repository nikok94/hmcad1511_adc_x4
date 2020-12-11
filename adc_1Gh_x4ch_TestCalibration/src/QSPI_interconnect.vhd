----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 05.10.2020 10:56:51
-- Design Name: 
-- Module Name: QSPI_interconnect - Behavioral
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
use ieee.std_logic_arith.all;

library work;
use work.spi_data_receiver;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity QSPI_interconnect is
  Generic(
    c_num_slave_port    : integer := 4;
    c_data_width        : integer := 64;
    c_command_width     : integer := 8;
    C_CPHA              : std_logic := '0';
    C_CPOL              : std_logic := '0';
    C_LSB_FIRST         : boolean := false
  );
  Port (
    slave_x_clk         : in std_logic_vector(c_num_slave_port - 1 downto 0);
    slave_x_ready       : out std_logic_vector(c_num_slave_port - 1 downto 0);
    slave_x_data        : in std_logic_vector(c_num_slave_port*c_data_width - 1 downto 0);
    slave_x_cs_up       : out std_logic_vector(c_num_slave_port - 1 downto 0);
    qspi_sio            : inout std_logic_vector(3 downto 0);
    qspi_sck            : in std_logic;
    qspi_cs             : in std_logic
  );
end QSPI_interconnect;

architecture Behavioral of QSPI_interconnect is
  constant num_stage        : integer := 3;
  constant C_NUM_QBURST     : integer := c_data_width / 4;
  signal qspi_t             : std_logic;
  signal sio                : std_logic_vector(3 downto 0);
  signal cmd                : std_logic_vector(c_command_width - 1 downto 0);
  signal cmd_d              : std_logic_vector(c_command_width - 1 downto 0);
  signal cmd_valid          : std_logic;
  --type sync_vect_type is array(0 to c_num_slave_port - 1) of std_logic_vector(num_stage - 1 downto 0);
  signal ready_sync_vect    : std_logic_vector(num_stage*c_num_slave_port - 1 downto 0);
  signal rst_sync_vect      : std_logic_vector(num_stage*c_num_slave_port - 1 downto 0);
  signal qcounter           : integer;
  signal ready              : std_logic;
  signal treg               : std_logic_vector(c_data_width - 1 downto 0);
  signal mode               : std_logic;
  signal ssck               : std_logic;
  signal slave_x_data_d     : std_logic_vector(c_num_slave_port*c_data_width - 1 downto 0);
  signal cmd_d_int          : integer;
  signal cmd_int            : integer;
begin

spi_data_receiver_inst : entity spi_data_receiver
  Generic map(
    C_CPOL        => C_CPHA,
    C_CPHA        => C_CPOL,
    C_LSB_FIRST   => false,
    C_D_WIDTH     => c_command_width
  )
  Port map(
    SCK           => qspi_sck,
    CS            => qspi_cs,
    MOSI          => qspi_sio(0),
    
    DATA          => cmd,
    VALID         => cmd_valid
  );


qspi_sio <= (others => 'Z') when qspi_t = '1' else sio;

cmd_d_int <= conv_integer(unsigned(cmd_d));
cmd_int <= conv_integer(unsigned(cmd));

ready_gen_proc : for i in 0 to c_num_slave_port - 1 generate
  process(qspi_t, slave_x_clk(i))
  begin
    if (qspi_t = '1') then
      slave_x_ready(i) <= '0';
      ready_sync_vect(i*num_stage + num_stage - 1 downto i*num_stage) <= (others => '0');
    elsif rising_edge(slave_x_clk(i)) then
      if (cmd_d_int = i) then
        ready_sync_vect(i*num_stage) <= ready;
      else
        ready_sync_vect(i*num_stage) <= '0';
      end if;
      ready_sync_vect(i*num_stage + num_stage - 1 downto i*num_stage + 1) <= ready_sync_vect(i*num_stage + num_stage - 2 downto i*num_stage);
      slave_x_ready(i) <= (not ready_sync_vect(i*num_stage + num_stage - 1)) and ready_sync_vect(i*num_stage + num_stage - 2);
    end if;
  end process;


  slave_x_cs_up_proc :
  process(slave_x_clk(i))
  begin
    if rising_edge(slave_x_clk(i)) then
      if (cmd_d_int = i) then
        rst_sync_vect(i*num_stage) <= qspi_cs;
      else
        rst_sync_vect(i*num_stage) <= '0';
      end if;
      rst_sync_vect(i*num_stage + num_stage - 1 downto i*num_stage + 1) <= rst_sync_vect(i*num_stage + num_stage - 2 downto i*num_stage);
      slave_x_cs_up(i) <= (not rst_sync_vect(i*num_stage + num_stage - 1)) and rst_sync_vect(i*num_stage + num_stage - 2);
    end if;
  end process;

end generate;

mode <= c_cpol XOR c_cpha;
  WITH mode SELECT
    ssck <= NOT qspi_sck WHEN '1',
            qspi_sck WHEN OTHERS;

--qspi_t_proc :
--  process(qspi_cs, qspi_sck) 
--  begin
--    if (qspi_cs = '1') then
--      qspi_t <= '1';
--    elsif falling_edge(qspi_sck) then
--      if (qspi_t = '1') then
--        if (cmd_valid = '1') then
--          cmd_d <= cmd;
--          qspi_t <= '0';
--        end if;
--      end if;
--    end if;
--  end process;

true_gen_proc : if C_LSB_FIRST = true generate
ready_proc:
  process(ssck, qspi_t) 
  begin
    if (qspi_cs = '1') then
      qspi_t <= '1';
      ready <= '0';
      qcounter <= C_NUM_QBURST;
      treg <= (others => 'Z');
    elsif falling_edge(ssck) then
      if (qspi_t = '1') then
        if (cmd_valid = '1') then
          treg <= slave_x_data(cmd_int*c_data_width + c_data_width - 1 downto conv_integer(cmd)*c_data_width);
          cmd_d <= cmd;
          qspi_t <= '0';
          ready <= '1';
          qcounter <= 0;
        end if;
      else
        if (qcounter < C_NUM_QBURST - 1) then
          qcounter <= qcounter + 1;
          treg(treg'length - 5 downto 0) <= treg(treg'length - 1 downto 4);
          treg(treg'length - 1 downto treg'length - 4) <= (others => '1');
          ready <= '0';
        else
          ready <= '1';
          qcounter <= 0;
          treg <= slave_x_data(cmd_d_int*c_data_width + c_data_width - 1 downto conv_integer(cmd_d)*c_data_width);
        end if;
      end if;
    end if;
  end process;
  sio <= treg(3 downto 0);
end generate;

false_gen_proc : if C_LSB_FIRST = false generate
ready_proc:
  process(ssck, qspi_t) 
  begin
    if (qspi_cs = '1') then
      qspi_t <= '1';
      ready <= '0';
      qcounter <= C_NUM_QBURST;
      treg <= (others => 'Z');
    elsif falling_edge(ssck) then
      if (qspi_t = '1') then
        if (cmd_valid = '1') then
          treg <= slave_x_data(cmd_int*c_data_width + c_data_width - 1 downto conv_integer(cmd)*c_data_width);
          cmd_d <= cmd;
          qspi_t <= '0';
          ready <= '1';
          qcounter <= 0;
        end if;
      else
        if (qcounter < C_NUM_QBURST - 1) then
          qcounter <= qcounter + 1;
          treg(treg'length - 1 downto 4) <= treg(treg'length - 5 downto 0);
          treg(3 downto 0) <= (others => '1');
          ready <= '0';
        else
          ready <= '1';
          qcounter <= 0;
          treg <= slave_x_data(cmd_d_int*c_data_width + c_data_width - 1 downto conv_integer(cmd_d)*c_data_width);
        end if;
      end if;
    end if;
  end process;
  sio <= treg(treg'length - 1 downto treg'length - 4);
end generate;

end Behavioral;
