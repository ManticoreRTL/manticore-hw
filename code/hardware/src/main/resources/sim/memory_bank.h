#ifndef __MANTICORE_MEMORY_BANK_H__
#define __MANITCORE_MEMORY_BANK_H__

class MemoryBank {

public:
  using word_t = uint64_t;
  MemoryBank(const std::string &name, uint64_t capacity = 8192)
      : m_name(name), m_storage((capacity - 1) / (sizeof(word_t)) + 1) {}

  uint64_t read_word(uintptr_t addr) const {
    check_address(addr);
    printf("address %lu base %lu\n", addr, get_base());
    uint64_t index = address_to_index(addr);
    return m_storage[index];
  }

  void write_word(uintptr_t addr, word_t word) {
    check_address(addr);
    uint64_t index = address_to_index(addr);
    m_storage[index] = word;
  }

  uintptr_t get_base() const { reinterpret_cast<uintptr_t>(m_storage.data()); }

  uint64_t address_to_index(uintptr_t addr) const {

    return (addr - get_base()) / sizeof(word_t);
  }

  word_t get_word(uint64_t index) { return m_storage[index]; }
  void set_word(uint64_t index, word_t value) { m_storage[index] = value; }

private:
  void check_address(uintptr_t addr) const {
    uintptr_t base_addr = get_base();
    uintptr_t last_address =
        (base_addr + m_storage.capacity() * sizeof(uint64_t)) - 1;
    if ((addr < base_addr) || (addr > last_address)) {
      printf("Address 0x%p out of range (0x%p, 0x%p) in %s\n",
             reinterpret_cast<void *>(addr),
             reinterpret_cast<void *>(base_addr),
             reinterpret_cast<void *>(last_address), m_name.c_str());
      throw std::out_of_range("Out of bound memory access");
    }
  }
  const std::string m_name;

  std::vector<word_t> m_storage;
};

#endif