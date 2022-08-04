library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;

entity async_ram_dual_port is

  generic (
    WIDTHA      : integer := 8;
    SIZEA       : integer := 256;
    ADDRWIDTHA  : integer := 8;
    WIDTHB      : integer := 32;
    SIZEB       : integer := 64;
    ADDRWIDTHB  : integer := 6
    );

  port (
    clk_a   : in  std_logic;
    clk_b   : in  std_logic;
    en_a    : in  std_logic;
    en_b    : in  std_logic;
    we_a    : in  std_logic;
    we_b    : in  std_logic;
    addr_a  : in  std_logic_vector(ADDRWIDTHA-1 downto 0);
    addr_b  : in  std_logic_vector(ADDRWIDTHB-1 downto 0);
    di_a    : in  std_logic_vector(WIDTHA-1 downto 0);
    di_b    : in  std_logic_vector(WIDTHB-1 downto 0);
    do_a    : out std_logic_vector(WIDTHA-1 downto 0);
    do_b    : out std_logic_vector(WIDTHB-1 downto 0)
    );

end async_ram_dual_port;

architecture behavioral of async_ram_dual_port is

  function max(L, R: INTEGER) return INTEGER is
  begin
      if L > R then
          return L;
      else
          return R;
      end if;
  end;

  function min(L, R: INTEGER) return INTEGER is
  begin
      if L < R then
          return L;
      else
          return R;
      end if;
  end;

  function log2 (val: INTEGER) return natural is
    variable res : natural;
  begin
        for i in 0 to 31 loop
            if (val <= (2**i)) then
                res := i;
                exit;
            end if;
        end loop;
        return res;
  end function Log2;

  constant minWIDTH : integer := min(WIDTHA,WIDTHB);
  constant maxWIDTH : integer := max(WIDTHA,WIDTHB);
  constant maxSIZE  : integer := max(SIZEA,SIZEB);
  constant RATIO : integer := maxWIDTH / minWIDTH;

  -- An asymmetric RAM is modeled in a similar way as a symmetric RAM, with an
  -- array of array object. Its aspect ratio corresponds to the port with the
  -- lower data width (larger depth)
  type ramType is array (0 to maxSIZE-1) of std_logic_vector(minWIDTH-1 downto 0);

  -- You need to declare ram as a shared variable when :
  --   - the RAM has two write ports,
  --   - the RAM has only one write port whose data width is maxWIDTH
  -- In all other cases, ram can be a signal.
  shared variable ram : ramType := (others => (others => '0'));
  
  signal read_a : std_logic_vector(WIDTHA-1 downto 0):= (others => '0');
  signal read_b : std_logic_vector(WIDTHB-1 downto 0):= (others => '0');
  signal reg_a  : std_logic_vector(WIDTHA-1 downto 0):= (others => '0');
  signal reg_b  : std_logic_vector(WIDTHB-1 downto 0):= (others => '0');

begin

  process (clk_a)
  begin
    if rising_edge(clk_a) then
      if en_a = '1' then
        if we_a = '1' then
          ram(conv_integer(addr_a)) := di_a;
          read_a <= di_a;
        else
		  read_a <= ram(conv_integer(addr_a));
        end if;
      end if;
      reg_a <= read_a;
    end if;
  end process;

  process (clk_b)
  begin
    if rising_edge(clk_b) then
      if en_b = '1' then        
        for i in 0 to RATIO-1 loop
          if we_b = '1' then
            ram(conv_integer(addr_b & conv_std_logic_vector(i,log2(RATIO))))
	          := di_b((i+1)*minWIDTH-1 downto i*minWIDTH);
          end if;
		  -- The read statement below is placed after the write statement on purpose
		  -- to ensure write-first synchronization through the variable mechanism
          read_b((i+1)*minWIDTH-1 downto i*minWIDTH)
	        <= ram(conv_integer(addr_b & conv_std_logic_vector(i,log2(RATIO))));
        end loop;
      end if;
      reg_b <= read_b;
    end if;
  end process;

  do_a <= reg_a;
  do_b <= reg_b;
  
end behavioral;