


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_unsigned.ALL;

library work;
use work.spi_data_receiver;

entity spifi_module is
    generic (
      C_CPHA            : std_logic := '0';
      C_CPOL            : std_logic := '0';
      C_LSB_FIRST       : boolean := false;
      C_NUM_QBURST      : integer := 8
    );
    Port ( 
      SCK           : in std_logic;
      CS            : in std_logic;

      PCS_I         : in  std_logic_vector(3 downto 0);
      PCS_O         : out std_logic_vector(3 downto 0);

      s_data        : in std_logic_vector(C_NUM_QBURST*4 - 1 downto 0);
      s_ready       : out std_logic;

      cmd_byte      : out std_logic_vector(7 downto 0);
      cmd_valid     : out std_logic
    );
end spifi_module;

architecture Behavioral of spifi_module is
  signal mode               : std_logic;
  signal ssck               : std_logic;
  signal ssck_rising_edge   : std_logic;
  signal ssck_falling_edge  : std_logic;
  signal ssck_sync_vector   : std_logic_vector(2 downto 0);
  signal qcounter           : integer;
  signal ready              : std_logic;
  signal treg               : std_logic_vector(C_NUM_QBURST*4 - 1 downto 0);

begin

spi_data_receiver_inst : entity spi_data_receiver
    Generic map(
      C_CPOL        => C_CPHA,
      C_CPHA        => C_CPOL,
      C_LSB_FIRST   => C_LSB_FIRST,
      C_D_WIDTH     => 8
    )
    Port map(
      SCK           => SCK,
      CS            => CS,
      MOSI          => PCS_I(0),
      
      DATA          => cmd_byte,
      VALID         => cmd_valid
    );

mode <= c_cpol XOR c_cpha;
  WITH mode SELECT
    ssck <= NOT SCK WHEN '1',
            SCK WHEN OTHERS;

true_gen_proc : if C_LSB_FIRST = true generate
ready_proc:
  process(ssck, cs) 
  begin
    if (cs = '1') then
      ready <= '0';
      qcounter <= C_NUM_QBURST;
      treg <= (others => '1');
    elsif falling_edge(ssck) then
      if (qcounter < C_NUM_QBURST - 1) then
        qcounter <= qcounter + 1;
        treg(treg'length - 5 downto 0) <= treg(treg'length - 1 downto 4);
        treg(treg'length - 1 downto treg'length - 4) <= (others => '1');
        ready <= '0';
      else
        ready <= '1';
        qcounter <= 0;
        treg <= s_data;
      end if;
    end if;
  end process;
  PCS_O <= treg(3 downto 0);
end generate;

false_gen_proc : if C_LSB_FIRST = false generate
ready_proc:
  process(ssck, cs) 
  begin
    if (cs = '1') then
      ready <= '0';
      qcounter <= C_NUM_QBURST;
      treg <= (others => '1');
    elsif falling_edge(ssck) then
      if (qcounter < C_NUM_QBURST - 1) then
        qcounter <= qcounter + 1;
        treg(treg'length - 1 downto 4) <= treg(treg'length - 5 downto 0);
        treg(3 downto 0) <= (others => '1');
        ready <= '0';
      else
        ready <= '1';
        qcounter <= 0;
        treg <= s_data;
      end if;
    end if;
  end process;
  PCS_O <= treg(treg'length - 1 downto treg'length - 4);
end generate;



s_ready <= ready;




end Behavioral;
