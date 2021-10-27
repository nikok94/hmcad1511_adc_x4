library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;
use ieee.math_real.all;

entity true_dpram_sclk is
  Generic (
    c_data_width  : integer := 8;
    c_data_num    : integer := 16
  );
  port
  (
    data    : in std_logic_vector(c_data_width - 1 downto 0);
    addr    : in std_logic_vector(natural(round(log2(real(c_data_num)))) - 1 downto 0);
    wea     : in std_logic;
    clk     : in std_logic;
    q       : out std_logic_vector(c_data_width - 1 downto 0)
  );
end true_dpram_sclk;

architecture rtl of true_dpram_sclk is
  
  -- Build a 2-D array type for the RAM
  type memory_t is array(c_data_num - 1 downto 0) of std_logic_vector(c_data_width - 1 downto 0);
  
  -- Declare the RAM
  signal ram   : memory_t;

begin

    -- Port A
    process(clk)
    begin
      if rising_edge(clk) then 
        if (wea = '1') then
            ram(conv_integer(addr)) <= data;
        end if;
        q <= ram(conv_integer(addr));
      end if;
    end process;

end rtl;
