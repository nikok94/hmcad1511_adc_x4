
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_unsigned.ALL;
use IEEE.math_real.ALL;


library work;
--use work.true_dpram_ss_clk;
use work.async_ram_dual_port;
--use work.blk_mem_gen_v7_3_0;

entity data_recorder is
  generic (
    c_max_num_data            : integer := 16;
    c_data_width              : integer := 64;
    c_cnt_width               : integer := 32
  );
  Port ( 
    rst                       : in std_logic;

    mark_delay                : in std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);
    mark_cnt                  : in std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);

    stop                      : in std_logic;
    stop_cnt                  : in std_logic_vector(c_cnt_width - 1 downto 0);

    s_clk                     : in std_logic;
    s_cnt                     : in std_logic_vector(c_cnt_width - 1 downto 0);
    s_data                    : in std_logic_vector(c_data_width - 1 downto 0);
    s_valid                   : in std_logic;
    s_ready                   : out std_logic;

    m_clk                     : in std_logic;
    m_en                      : in std_logic;
    m_data                    : out std_logic_vector(c_data_width - 1 downto 0);
    m_valid                   : out std_logic;
    m_ready                   : in std_logic

  );
end data_recorder;

architecture Behavioral of data_recorder is
  signal state_a                : std_logic_vector(7 downto 0):= (others => '0');
  constant state_cnt_max        : std_logic_vector(natural(round(log2(real(c_max_num_data)))) - 1 downto 0):= (others => '1');
  signal state_a_cnt            : std_logic_vector(natural(round(log2(real(c_max_num_data)))) - 1 downto 0):= (others => '0');
  signal addr_a                 : std_logic_vector(natural(round(log2(real(c_max_num_data)))) - 1 downto 0);
  signal we_a                   : std_logic:= '0';
  signal en_a                   : std_logic:= '0';
  signal valid                  : std_logic:= '0';
  


  constant null_data            : std_logic_vector(c_data_width - 1 downto 0):= (others => '0');
  type state_b_type          is (IDLE, WAIT_DB, WAIT_M_READY0, ADDR_B_INC0, WAIT_DB1, WAIT_M_READY1, ADDR_B_INC1, CONT);
  signal state_b                : state_b_type;
  signal addr_b                 : std_logic_vector(natural(round(log2(real(c_max_num_data)))) - 1 downto 0):= (others => '0');
  signal en_b                   : std_logic:= '0';
begin
m_valid <= valid;

--addr_a <= state_a_cnt;

process(s_clk, rst)
begin
  if (rst = '1') then
    state_a <= x"00";
    state_a_cnt <= (others => '0');
    en_a <= '0';
    s_ready <= '0';
    addr_a <= (others => '0');
  elsif rising_edge(s_clk) then
    case (state_a) is
      when x"00" => -- wait rst cnt
        if (s_valid = '1') then
          if (s_cnt = x"0000_0001") then
            state_a <= x"01";
            state_a_cnt <= (others => '0');
          end if;
        end if;
        en_a <= '0';
        s_ready <= '0';
      when x"01" => -- wait mask trigger
        if (s_valid = '1') then
          if (state_a_cnt < mark_delay) then
            state_a_cnt <= state_a_cnt + 1;
          else
            state_a_cnt <= (others => '0');
            addr_a <= (others => '0');
            state_a <= x"02";
          end if;
        end if;
        en_a <= '1'; 
      when x"02" => -- mask recording
        if (s_valid = '1') then
          if (addr_a >= mark_cnt) then
            state_a <= x"03";
          end if;
          addr_a <= addr_a + 1;
        end if;
      when x"03" => -- wait trigger
        if (s_valid = '1') then
          if (addr_a < state_cnt_max) then
            addr_a <= addr_a + 1;
          else
            addr_a <= mark_cnt;
          end if;
        end if;
        if (stop = '1') then
          state_a <= x"04";
          s_ready <= '0';
        else
          s_ready <= '1';
        end if;
      when x"04" =>  
        if (s_valid = '1') then
          if (addr_a < state_cnt_max) then
            addr_a <= addr_a + 1;
          else
            addr_a <= mark_cnt;
          end if;

          if (s_cnt >= stop_cnt) then
            state_a <= x"05";
--            state_a_cnt <= addr_a;
            en_a <= '0';
          end if;
        end if;
      when x"05" =>  -- data recording
        en_a <= '0';
      when others => --ready
    end case;
  end if;
end process;

--dpram_inst : entity true_dpram_ss_clk
--  Generic map(
--    c_data_width  => c_data_width,
--    c_data_num    => c_max_num_data
--  )
--  Port map
--  (
--    data    => s_data,
--    addr_a    => addr_a,
--    wea     => we_a,
--    s_clk     => s_clk,
--    q       => m_data
--  );

process(m_clk, rst)
begin
  if (rst = '1') then
    state_b <= IDLE;
    valid <= '0';
  elsif rising_edge(m_clk) then
    case (state_b) is
      when IDLE => -- ready
        if (state_a = x"05") then
          state_b <= WAIT_DB;
          en_b <= '1';
        else
          en_b <= '0';
        end if;
        addr_b <= (others => '0');
      when WAIT_DB =>
        state_b <= WAIT_M_READY0;
      when WAIT_M_READY0 =>
        if ((valid = '1') and (m_ready = '1')) then
          valid <= '0';
          addr_b <= addr_b + 1;
          state_b <= ADDR_B_INC0;
        else
          valid <= '1';
        end if;
      when ADDR_B_INC0 =>
        if (addr_b < mark_cnt) then
          state_b <= WAIT_M_READY0;
        else
          --if (addr_a < state_cnt_max) then
          --  addr_b <= addr_a + 1;
          --else
          --  addr_b <= mark_cnt;
          --end if;
          addr_b <= addr_a;
          state_b <= WAIT_DB1;
        end if;
      when WAIT_DB1 =>
        state_b <= WAIT_M_READY1;
      when WAIT_M_READY1 =>
        if ((valid = '1') and (m_ready = '1')) then
          valid <= '0';
          if (addr_b < state_cnt_max) then
            addr_b <= addr_b + 1;
          else
            addr_b <= mark_cnt;
          end if;
          state_b <= ADDR_B_INC1;
        else
          valid <= '1';
        end if;
      when ADDR_B_INC1 =>
        if (addr_b = addr_a) then
          state_b <= CONT;
        else
          state_b <= WAIT_M_READY1;
        end if;
      when others => --ready
        en_b <= '0';
    end case;
  end if;
end process;

we_a <= s_valid;

dpram_inst : entity async_ram_dual_port
  generic map (
    WIDTHA      => c_data_width,
    SIZEA       => c_max_num_data,
    ADDRWIDTHA  => addr_a'length,
    WIDTHB      => c_data_width,
    SIZEB       => c_max_num_data,
    ADDRWIDTHB  => addr_b'length
    )
  port map(
    clk_a       => s_clk,
    clk_b       => m_clk,
    en_a        => en_a,
    en_b        => en_b,
    we_a        => we_a,
    we_b        => '0',
    addr_a      => addr_a,
    addr_b      => addr_b,
    di_a        => s_data,
    di_b        => null_data,
    do_a        => open,
    do_b        => m_data
    );

end Behavioral;
