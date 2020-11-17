


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_unsigned.ALL;

library work;
use work.spi_data_receiver;
use work.spi_data_transceiver;


entity spi_master is
    generic (
      C_CPHA            : std_logic := '0';
      C_CPOL            : std_logic := '0';
      C_LSB_FIRST       : boolean := false
      
    );
    Port ( 
      SCK           : in std_logic;
      CS            : in std_logic;

      MISO_I        : in std_logic;
      MISO_O        : out std_logic;
      MISO_T        : out std_logic;
      
      MOSI_I        : in std_logic;
      MOSI_O        : out std_logic;
      MOSI_T        : out std_logic;

      m_fcb_clk     : in std_logic;
      m_fcb_areset  : in std_logic;
      m_fcb_addr    : out std_logic_vector(8 - 1 downto 0);
      m_fcb_wrdata  : out std_logic_vector(16 - 1 downto 0);
      m_fcb_wrreq   : out std_logic;
      m_fcb_wrack   : in std_logic;
      m_fcb_rddata  : in std_logic_vector(16 - 1 downto 0);
      m_fcb_rdreq   : out std_logic;
      m_fcb_rdack   : in std_logic
    );
end spi_master;

architecture Behavioral of spi_master is
    constant c_fcb_addr_width           : integer := 8;
    constant c_fcb_data_width           : integer := 16;
    constant spi_data_whidth            : integer := 8;
    type spi_state_machine      is (idle, addr_st, fcb_read_st, get_data_st, fcb_write_st, set_data_st);
    signal state, next_state            : spi_state_machine;
    signal address                      : std_logic_vector(c_fcb_addr_width - 1 downto 0);
    signal wr_data                      : std_logic_vector(c_fcb_data_width - 1 downto 0);
    signal rd_data                      : std_logic_vector(c_fcb_data_width - 1 downto 0);
    signal spi_rec_data                 : std_logic_vector(spi_data_whidth - 1 downto 0);
    signal spi_rec_valid                : std_logic;
    signal spi_rec_valid_d0             : std_logic;
    signal spi_rec_valid_d1             : std_logic;
    signal spi_rec_irq                  : std_logic;
    signal spi_trans_data               : std_logic_vector(spi_data_whidth - 1 downto 0);
    signal spi_trans_start              : std_logic;
    signal spi_trans_ready              : std_logic;
    signal spi_trans_busy               : std_logic;
    signal spi_trans_busy_d0            : std_logic;
    signal spi_trans_busy_d1            : std_logic;
    signal spi_trans_error              : std_logic;
    signal rst                          : std_logic;
    signal clk                          : std_logic;
    signal rd_req                       : std_logic;
    signal BUSY_d0                      : std_logic;
    signal BUSY_d1                      : std_logic;
    signal EDGE                         : std_logic;
    type spi_rec_buff_type  is array (2 downto 0) of std_logic_vector(spi_data_whidth - 1 downto 0);
    signal spi_buff                     : spi_rec_buff_type;
    signal spi_buff_addr                : integer range 0 to 2;
    signal tr_data                      : std_logic_vector(spi_data_whidth - 1 downto 0);
    signal tr_data1                     : std_logic_vector(spi_data_whidth - 1 downto 0);
    signal spi_tr_irq                   : std_logic;
    signal t_data                       : std_logic_vector(spi_data_whidth - 1 downto 0);
    signal tload                        : std_logic := '0';

begin
rst <= m_fcb_areset;
clk <= m_fcb_clk;

spi_receiver_inst : entity spi_data_receiver
    Generic map(
      C_CPHA        => C_CPHA,
      C_CPOL        => C_CPOL,
      C_LSB_FIRST   => C_LSB_FIRST,
      C_D_WIDTH     => spi_data_whidth
    )
    Port map(
      SCK          => SCK ,
      CS           => CS  ,
      MOSI         => mosi_i,
      
      data         => spi_rec_data,
      valid        => spi_rec_valid
    );

spi_transceiver_ist : entity spi_data_transceiver
    Generic map(
      C_CPHA        => C_CPHA,
      C_CPOL        => C_CPOL,
      C_LSB_FIRST   => C_LSB_FIRST,
      C_D_WIDTH     => spi_data_whidth
    )
    Port map(
      SCK           => SCK ,
      CS            => CS  ,
      MISO          => miso_o,

      DATA          => t_data,
      BUSY          => spi_trans_busy,
      LOAD          => tload
    );

t_data <= spi_buff(spi_buff_addr);

spi_rec_irq_proc :
  process(m_fcb_clk)
  begin
    if rising_edge(m_fcb_clk) then
      spi_rec_valid_d0 <= spi_rec_valid;
      spi_rec_valid_d1 <= spi_rec_valid_d0;
      spi_rec_irq <= (not spi_rec_valid_d1) and spi_rec_valid_d0;
      
      spi_trans_busy_d0 <= spi_trans_busy;
      spi_trans_busy_d1 <= spi_trans_busy_d0;
      spi_tr_irq <= spi_trans_busy_d1 and (not spi_trans_busy_d0);
    end if;
  end process;

m_fcb_addr <= '0' & spi_buff(0)(spi_data_whidth - 2 downto 0);
m_fcb_wrdata <= spi_buff(2) & spi_buff(1);

state_proc :
  process(cs, rst, clk)
  begin
    if ((rst = '1') or (cs = '1')) then
      state <= idle;
      spi_buff_addr <= 0;
      m_fcb_rdreq <= '0';
      m_fcb_wrreq <= '0';
      tload <= '0';
    elsif rising_edge(clk) then
      case (state) is
        when idle =>
          if (spi_rec_irq = '1') then
            spi_buff(spi_buff_addr) <= spi_rec_data;
            spi_buff_addr <= spi_buff_addr + 1;
            if (spi_rec_data(spi_rec_data'length - 1) = '1') then
              state <= fcb_read_st;
              m_fcb_rdreq <= '1';
            else
              state <= get_data_st;
            end if;
          end if;
        when get_data_st =>
          if (spi_rec_irq = '1') then
            spi_buff(spi_buff_addr) <= spi_rec_data;
            if (spi_buff_addr = 2) then
              state <= fcb_write_st;
              m_fcb_wrreq <= '1';
              spi_buff_addr <= 0;
            else
              spi_buff_addr <= spi_buff_addr + 1;
            end if;
          end if;
        when fcb_write_st =>
          if (m_fcb_wrack = '1') then
            state <= idle;
            m_fcb_wrreq <= '0';
            spi_buff(0) <= (others => '0');
            spi_buff(1) <= (others => '0');
            spi_buff(2) <= (others => '0');
          end if;
        when fcb_read_st =>
          if (m_fcb_rdack = '1') then
            m_fcb_rdreq <= '0';
            spi_buff(1) <= m_fcb_rddata(2*spi_data_whidth - 1 downto spi_data_whidth);
            spi_buff(2) <= m_fcb_rddata(spi_data_whidth - 1 downto 0);
            spi_buff_addr <= 1;
            state <= set_data_st;
            tload <= '1';
          end if;
        when set_data_st => 
          if (spi_tr_irq = '1') then
            if (spi_buff_addr = 2) then
              state <= idle;
              spi_buff(0) <= (others => '0');
              spi_buff(1) <= (others => '0');
              spi_buff(2) <= (others => '0');
              spi_buff_addr <= 0;
            else
              spi_buff_addr <= spi_buff_addr + 1;
              tload <= '1';
            end if;
          else
            tload <= '0';
          end if;
        when others =>
         spi_buff_addr <= 0;
         m_fcb_rdreq <= '0';
         m_fcb_wrreq <= '0';
         spi_buff(0) <= (others => '0');
         spi_buff(1) <= (others => '0');
         spi_buff(2) <= (others => '0');
         state <= idle;
      end case;
    end if;
  end process;
end Behavioral;
