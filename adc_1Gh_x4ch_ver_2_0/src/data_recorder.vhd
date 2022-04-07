
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_unsigned.ALL;
use IEEE.math_real.ALL;


library work;
use work.true_dpram_sclk;
--use work.blk_mem_gen_v7_3_0;

entity data_recorder is
    generic (
      c_max_num_data            : integer := 16;
      c_data_width              : integer := 64;
      c_start_delay             : integer := 9
    );
    Port ( 
      rst                       : in std_logic;
      clk                       : in std_logic;

      mark_delay                : in std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);
      mark_length               : in std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);

      start                     : in std_logic;
      num_data                  : in std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);
      start_offset              : in std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);

      s_data                    : in std_logic_vector(c_data_width - 1 downto 0);
      s_valid                   : in std_logic;
      s_ready                   : out std_logic;

      m_data                    : out std_logic_vector(c_data_width - 1 downto 0);
      m_valid                   : out std_logic;
      m_ready                   : in std_logic;
      
      compleat                  : out std_logic
    );
end data_recorder;

architecture Behavioral of data_recorder is
  signal state                  : std_logic_vector(7 downto 0):= (others => '0');
  constant state_cnt_max        : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0):= (others => '1');
  signal n_count                : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0):= (others => '1');
  signal n_count_max            : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0):= (others => '1');
  signal state_cnt              : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);
  signal addr                   : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);
  signal we_a                   : std_logic:= '0';
  signal valid                  : std_logic:= '0';
  
  

begin
m_valid <= valid;

process(clk, rst)
begin
  if (rst = '1') then
    state <= x"00";
    we_a <= '0';
    valid <= '0';
    state_cnt <= (others => '0');
    compleat <= '0';
  elsif rising_edge(clk) then
    case (state) is
      when x"00" =>
        state_cnt <= (others => '0');
        compleat <= '0';
        if (start = '1') then
          state <= x"01";
          n_count_max <= state_cnt_max - c_start_delay;
        end if;
        we_a <= '0';
        valid <= '0';
      when x"01" => -- wait until the impulse passes through the path ADC
        if (state_cnt < mark_delay) then
          state_cnt <= state_cnt + 1;
        else
          state <= x"02";
          n_count_max <= n_count_max - start_offset;
          state_cnt <= (others => '0');
          we_a <= '1';
        end if;
      when x"02" => -- impulse recording 
        state_cnt <= state_cnt + 1;
        if (state_cnt >= mark_length) then
          state <= x"03";
          n_count_max <= n_count_max - mark_length;
        end if;
      when x"03" =>
        if (state_cnt >= state_cnt_max) then
          state_cnt <= mark_length;
        else
          state_cnt <= state_cnt + 1;
        end if;
        if (start = '1') then
          state <= x"04";
          n_count <= (others => '0');
        end if;
      when x"04" =>
        if (n_count < n_count_max) then
          n_count <= n_count + 1;
        else
          state <= x"05";
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
        state <= x"06";
      when x"06" =>
        state <= x"07";
      when x"07" =>
        if ((valid = '1') and (m_ready = '1')) then
          valid <= '0';
          state_cnt <= state_cnt + 1;
          state <= x"08";
        else
          valid <= '1';
        end if;
      when x"08" =>
        if (state_cnt < mark_length) then
          state <= x"07";
        else
          state_cnt <= n_count;
          state <= x"09";
        end if;
      when x"09" =>
        if ((valid = '1') and (m_ready = '1')) then
          valid <= '0';
          if (state_cnt = state_cnt_max) then
            state_cnt <= mark_length;
          else
            state_cnt <= state_cnt + 1;
          end if;
          state <= x"0A";
        else
          valid <= '1';
        end if;
      when x"0A" =>
        if (state_cnt = n_count_max) then
          state <= x"0B";
        else
          state <= x"09";
        end if;
      when others =>
        compleat <= '1';
    end case;
  end if;
end process;

addr <= state_cnt;

dpram_inst : entity true_dpram_sclk
  Generic map(
    c_data_width  => c_data_width,
    c_data_num    => c_max_num_data
  )
  Port map
  (
    data    => s_data,
    addr    => addr,
    wea     => we_a,
    clk     => clk,
    q       => m_data
  );

end Behavioral;
