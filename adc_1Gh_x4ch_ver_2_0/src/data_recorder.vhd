
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
      c_start_delay             : integer := 9
    );
    Port ( 
      rst                       : in std_logic;

      mark_delay                : in std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);
      mark_length               : in std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);

      start                     : in std_logic;
      num_data                  : in std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);
      start_offset              : in std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);

      s_clk                     : in std_logic;
      s_en                      : in std_logic;
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
  signal n_count                : std_logic_vector(natural(round(log2(real(c_max_num_data)))) - 1 downto 0):= (others => '1');
  signal n_count_max            : std_logic_vector(natural(round(log2(real(c_max_num_data)))) - 1 downto 0):= (others => '1');
  signal state_cnt              : std_logic_vector(natural(round(log2(real(c_max_num_data)))) - 1 downto 0);
  signal addr_a                 : std_logic_vector(natural(round(log2(real(c_max_num_data)))) - 1 downto 0);
  signal we_a                   : std_logic:= '0';
  signal valid                  : std_logic:= '0';
  
  constant null_data            : std_logic_vector(c_data_width - 1 downto 0):= (others => '0');
  signal state_b                : std_logic_vector(7 downto 0):= (others => '0');
  signal we_b                   : std_logic:= '0';
  signal addr_b                 : std_logic_vector(natural(round(log2(real(c_max_num_data)))) - 1 downto 0);
  
  

begin
m_valid <= valid;

process(s_clk, rst)
begin
  if (rst = '1') then
    state_a <= x"00";
    we_a <= '0';
    state_cnt <= (others => '0');
  elsif rising_edge(s_clk) then
    case (state_a) is
      when x"00" =>
        state_cnt <= (others => '0');
        if (start = '1') then
          state_a <= x"01";
          n_count_max <= state_cnt_max - c_start_delay;
        end if;
        we_a <= '0';
      when x"01" => -- wait until the impulse passes through the path ADC
        if (state_cnt < mark_delay) then
          state_cnt <= state_cnt + 1;
        else
          state_a <= x"02";
          n_count_max <= n_count_max - start_offset;
          state_cnt <= (others => '0');
          we_a <= '1';
        end if;
      when x"02" => -- mask recording 
        state_cnt <= state_cnt + 1;
        if (state_cnt >= mark_length) then
          state_a <= x"03";
          n_count_max <= n_count_max - mark_length;
        end if;
      when x"03" => -- wait trigger
        if (state_cnt >= state_cnt_max) then
          state_cnt <= mark_length;
        else
          state_cnt <= state_cnt + 1;
        end if;
        if (start = '1') then
          state_a <= x"04";
          n_count <= (others => '0');
        end if;
      when x"04" => -- data recording
        if (n_count < n_count_max) then
          n_count <= n_count + 1;
        else
          state_a <= x"05";
          we_a <= '0';
        end if;
        if (state_cnt = state_cnt_max) then
          state_cnt <= mark_length;
        else
          state_cnt <= state_cnt + 1;
        end if;
      when x"05" =>
        state_cnt <= (others => '0');
        n_count <= state_cnt;
        n_count_max <= state_cnt;
        state_a <= x"06";
      when x"06" =>
        state_a <= x"07";
--      when x"07" => -- ready
--        if ((valid = '1') and (m_ready = '1')) then
--          valid <= '0';
--          state_cnt <= state_cnt + 1;
--          state_a <= x"08";
--        else
--          valid <= '1';
--        end if;
--      when x"08" =>
--        if (state_cnt < mark_length) then
--          state_a <= x"07";
--        else
--          state_cnt <= n_count;
--          state_a <= x"09";
--        end if;
--      when x"09" =>
--        if ((valid = '1') and (m_ready = '1')) then
--          valid <= '0';
--          if (state_cnt = state_cnt_max) then
--            state_cnt <= mark_length;
--          else
--            state_cnt <= state_cnt + 1;
--          end if;
--          state_a <= x"0A";
--        else
--          valid <= '1';
--        end if;
--      when x"0A" =>
--        if (state_cnt = n_count_max) then
--          state_a <= x"0B";
--        else
--          state_a <= x"09";
--        end if;
      when others => --ready
    end case;
  end if;
end process;

addr_a <= state_cnt;

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
    state_b <= x"00";
    we_b <= '0';
    valid <= '0';
  elsif rising_edge(m_clk) then
    case (state_b) is
      when x"00" => -- ready
        if (state_a = x"07") then
          state_b <= x"01";
        end if;
        addr_b <= (others => '0');
      when x"01" =>
        if ((valid = '1') and (m_ready = '1')) then
          valid <= '0';
          addr_b <= addr_b + 1;
          state_b <= x"02";
        else
          valid <= '1';
        end if;
      when x"02" =>
        if (addr_b < mark_length) then
          state_b <= x"01";
        else
          addr_b <= n_count;
          state_b <= x"03";
        end if;
      when x"03" =>
        if ((valid = '1') and (m_ready = '1')) then
          valid <= '0';
          if (addr_b = state_cnt_max) then
            addr_b <= mark_length;
          else
            addr_b <= addr_b + 1;
          end if;
          state_b <= x"04";
        else
          valid <= '1';
        end if;
      when x"04" =>
        if (addr_b = n_count_max) then
          state_b <= x"05";
        else
          state_b <= x"03";
        end if;
      when others => --ready
    end case;
  end if;
end process;

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
    en_a        => s_en,
    en_b        => m_en,
    we_a        => we_a,
    we_b        => we_b,
    addr_a      => addr_a,
    addr_b      => addr_b,
    di_a        => s_data,
    di_b        => null_data,
    do_a        => open,
    do_b        => m_data
    );

end Behavioral;
