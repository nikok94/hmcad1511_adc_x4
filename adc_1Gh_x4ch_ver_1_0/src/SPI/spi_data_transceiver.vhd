library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_unsigned.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;


entity spi_data_transceiver is
    Generic(
      C_CPOL        : STD_LOGIC := '0';  --spi clock polarity mode
      C_CPHA        : STD_LOGIC := '0';  --spi clock phase mode
      C_LSB_FIRST   : boolean   := true;
      C_D_WIDTH     : integer := 8;
      C_BURST_WIDTH : integer   := 8
    );
    Port (
      SCK           : in std_logic;
      CS            : in std_logic;
      MISO          : out std_logic;
      
      DATA          : in std_logic_vector(C_D_WIDTH - 1 downto 0);
      BUSY          : out std_logic;
      LOAD          : in std_logic
    );
end spi_data_transceiver;

architecture Behavioral of spi_data_transceiver is

  signal mode           : std_logic;
  signal ssck           : std_logic;
  signal output_reg     : std_logic_vector(C_D_WIDTH - 1 downto 0);
  signal sck_counter    : integer range 0 to C_D_WIDTH - 1;
  signal is_busy        : std_logic;

begin

mode <= c_cpol XOR c_cpha;
  WITH mode SELECT
    ssck <= NOT SCK WHEN '1',
            SCK WHEN OTHERS;

c_msb_first_gen : if c_lsb_first /= true generate

output_reg_proc :
  process(ssck, LOAD, is_busy)
  begin
    if ((LOAD = '1') and (is_busy = '0')) then
      output_reg <= DATA;
    elsif falling_edge(ssck) then
      output_reg(output_reg'length - 1 downto 1) <= output_reg(output_reg'length - 2 downto 0);
      output_reg(0) <= '0';
    end if;
  end process;
  MISO <= output_reg(output_reg'length - 1);
end generate c_msb_first_gen;

c_lsb_first_gen : if c_lsb_first = true generate

output_reg_proc :
  process(ssck, LOAD)
  begin
    if (LOAD = '1') then
      output_reg <= DATA;
    elsif falling_edge(ssck) then
      output_reg(output_reg'length - 2 downto 0) <= output_reg(output_reg'length - 1 downto 1);
      output_reg(output_reg'length - 1) <= '0';
    end if;
  end process;
  MISO <= output_reg(0);
end generate c_lsb_first_gen;

sck_counter_proc :
  process(ssck, CS)
  begin
    if (CS = '1') then
      sck_counter <= 0;
      is_busy <= '0';
    elsif falling_edge(ssck) then
      if (sck_counter = C_D_WIDTH - 1) then
        sck_counter <= 0;
        is_busy <= '0';
      else
        sck_counter <= sck_counter + 1;
        is_busy <= '1';
      end if;
    end if;
  end process;
BUSY <= is_busy;

end Behavioral;
