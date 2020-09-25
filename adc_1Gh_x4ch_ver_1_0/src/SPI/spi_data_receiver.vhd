library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_unsigned.ALL;

-- spi receiver module
-- after CS drops to 0,  data of size C_D_WIDTH is valid every C_D_WIDTH clock cycle SCK

entity spi_data_receiver is
    Generic(
      C_CPOL        : STD_LOGIC := '0';  --spi clock polarity mode
      C_CPHA        : STD_LOGIC := '0';  --spi clock phase mode
      C_LSB_FIRST   : boolean   := true;
      C_D_WIDTH     : integer := 8
    );
    Port (
      SCK           : in std_logic;
      CS            : in std_logic;
      MOSI          : in std_logic;
      
      DATA          : out std_logic_vector(C_D_WIDTH - 1 downto 0);
      VALID         : out std_logic
    );
end spi_data_receiver;

architecture Behavioral of spi_data_receiver is

  signal mode           : std_logic;
  signal ssck           : std_logic;
  signal input_reg      : std_logic_vector(C_D_WIDTH - 1 downto 0);
  signal sck_counter    : integer range 0 to C_D_WIDTH - 1 := 0;

begin

mode <= c_cpol XOR c_cpha;
  WITH mode SELECT
    ssck <= NOT SCK WHEN '1',
            SCK WHEN OTHERS;

c_msb_first_gen : if c_lsb_first /= true generate

input_reg_proc :
  process(ssck)
  begin
    if rising_edge(ssck) then
      input_reg(input_reg'length - 1 downto 1) <= input_reg(input_reg'length - 2 downto 0);
      input_reg(0) <= MOSI;
    end if;
  end process;

end generate c_msb_first_gen;

c_lsb_first_gen : if c_lsb_first = true generate

input_reg_proc :
  process(ssck)
  begin
    if rising_edge(ssck) then
      input_reg(input_reg'length - 2 downto 0) <= input_reg(input_reg'length - 1 downto 1);
      input_reg(input_reg'length - 1) <= MOSI;
    end if;
  end process;

end generate c_lsb_first_gen;

sck_counter_proc :
  process(ssck, CS)
  begin
    if (CS = '1') then
      sck_counter <= 0;
      VALID <= '0';
    elsif rising_edge(ssck) then
      if (sck_counter = C_D_WIDTH - 1) then
        sck_counter <= 0;
        VALID <= '1';
      else
        sck_counter <= sck_counter + 1;
        VALID <= '0';
      end if;
    end if;
  end process;

DATA <= input_reg;

end Behavioral;
