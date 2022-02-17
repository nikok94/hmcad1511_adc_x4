
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

<<<<<<< HEAD
=======
      mark_delay                : in std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);
      mark_length               : in std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);

>>>>>>> Iran2021
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
<<<<<<< HEAD
  signal addr_a                 : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0) := (others => '1');
  signal addr_b                 : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0) := (others => '1');
  signal addr                   : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0) := (others => '1');
  signal we_a                   : std_logic:= '0';
  signal we_b                   : std_logic:= '0';
  signal data_a                 : std_logic_vector(c_data_width - 1 downto 0):= (others => '0');
  signal data_b                 : std_logic_vector(c_data_width - 1 downto 0):= (others => '0');
--  signal q_a                    : std_logic_vector(c_data_width - 1 downto 0);
  signal q_b                    : std_logic_vector(c_data_width - 1 downto 0);
  type sm                   is (idle_st, start_st, record_st, ready_st, push_st, addr_edge_st, addr_fall_st, ready_st1);
  signal state, next_state      : sm;
  signal counter_a              : std_logic_vector(natural(round(log2(real(c_max_num_data)))) - 1 downto 0);
  signal counter_b              : std_logic_vector(natural(round(log2(real(c_max_num_data)))) - 1 downto 0);
  signal valid                  : std_logic := '0';
  signal ready                  : std_logic;
  signal count_a_max            : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);
  signal count_b_max            : std_logic_vector(natural(round(log2(real(c_max_num_data))))-1 downto 0);
  signal compl                  : std_logic;

begin

sync_proc :
  process(clk, rst)
  begin
    if (rst = '1') then
      state <= idle_st;
    elsif rising_edge(clk) then
      state <= next_state;
    end if;
  end process;

next_state_proc :
  process(state, start, counter_a, count_a_max, compl, m_ready, counter_b, count_b_max)
  begin
    next_state <= state;
    case (state) is
      when idle_st =>
        next_state <= start_st;
      when start_st =>
        if (start = '1') then
          next_state <= record_st;
        end if;
      when record_st =>
        if (counter_a >= count_a_max - 1) then
           next_state <= ready_st;
        end if;
      when ready_st =>
        if (m_ready = '1') then
          next_state <= addr_edge_st;
        end if;
      when addr_edge_st =>
        if (compl = '1') then
          next_state <= idle_st;
        elsif (m_ready = '0') then
          next_state <= ready_st;
        else
          next_state <= push_st;
        end if;
      when push_st =>
        if (compl = '1') then
          next_state <= idle_st;
        elsif (m_ready = '0') then
          next_state <= ready_st;
        end if;
      when others =>
        next_state <= idle_st;
    end case;
  end process;

out_proc :
  process(state, addr_a, addr_b)
  begin
    valid <= '0';
    ready <= '0';
    addr <= addr_b;
    compleat <= '0';
    case (state) is
      when idle_st => 
        compleat <= '1';
      when start_st =>
        ready <= '1';
        addr <= addr_a;
      when record_st =>
        ready <= '1';
        addr <= addr_a;
      when ready_st =>
        valid <= '1';
      when push_st => 
        valid <= '1';
      when others =>
    end case;
  end process;

m_valid <= valid;
s_ready <= ready;
we_a <= s_valid and ready;

addr_a_proc :
  process(clk, state)
  begin
    if (state = idle_st) then
      addr_a <= (others => '0');
    elsif rising_edge(clk) then
      if (we_a = '1') then
        addr_a <= addr_a + 1;
      end if;
    end if;
  end process;

addr_b_proc :
  process(state, clk)
  begin
    if (state = idle_st) then
      addr_b <= (others => '0');
    elsif rising_edge(clk) then
      if (state = start_st) and (start = '1') then
        addr_b <= addr_a - start_offset - c_start_delay + 1;
        count_a_max <= num_data - start_offset - c_start_delay;
        count_b_max <= num_data;
      else
        if ((m_ready = '1') and (valid = '1'))then
          addr_b <= addr_b + 1;
        elsif (state = push_st) then
          addr_b <= addr_b - 1;
        end if;
      end if;
    end if;
  end process;

counter_a_proc :
  process(state, clk)
  begin
    if (state /= record_st) then
      counter_a <= (others => '0');
    elsif rising_edge(clk) then
      if (we_a = '1') then
        counter_a <= counter_a + 1;
      end if;
    end if;
  end process;

counter_b_proc :
  process(state, clk)
  begin
    if (state = idle_st) then
      counter_b <= (others => '0');
      compl <= '0';
    elsif rising_edge(clk) then
      if (valid = '1') and (m_ready = '1') then
        if (counter_b < count_b_max - 1) then
          counter_b <= counter_b + 1;
        else
          compl <= '1';
        end if;
      end if;
    end if;
  end process;

--mem_ist : ENTITY blk_mem_gen_v7_3_0
--  PORT map(
--    clka    => clk,
--    wea(0)  => we_a,
--    addra   => addr,
--    dina    => s_data,
--    douta   => m_data
--  );

=======
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
          n_count_max <= state_cnt_max - start_offset;
        end if;
        we_a <= '0';
        valid <= '0';
      when x"01" =>
        if (state_cnt < mark_delay) then
          state_cnt <= state_cnt + 1;
        else
          state <= x"02";
          n_count_max <= n_count_max-c_start_delay;
          state_cnt <= (others => '0');
          we_a <= '1';
        end if;
      when x"02" =>
        state_cnt <= state_cnt + 1;
        if (state_cnt >= mark_length) then
          state <= x"03";
          n_count_max <= n_count_max - mark_length;
        end if;
      when x"03" =>
        if (state_cnt = state_cnt_max) then
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
>>>>>>> Iran2021

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
