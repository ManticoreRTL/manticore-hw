#ifndef __MANTICORE_COSIM_H__
#define __MANTICORE_COSIM_H__
#include "VManticoreKernel.h"
#include "VManticoreKernel__Dpi.h"
#include "memory_bank.h"
#include <memory>

class ManticoreKernelSimulator {
public:

    ManticoreKernelSimulator() {}

    uint32_t write_register(int offset, uint32_t value) {

        int time_out = 0;
        // write and axi slave register
        m_kernel->s_axi_control_AWADDR = offset;
        m_kernel->s_axi_control_AWVALID = 1;
        m_kernel->eval();
        while(m_kernel->s_axi_control_AWREADY == 0) {
            tick();
            
            if (time_out == 20) {
                printf("Timed-out writing axi slave register after %d cycles (offset = %d and value = %d)", time_out, offset, value);
                throw std::runtime_error("Timed-out on axi slave write")
            }
            time_out ++;
        }
        m_kernel

    }
    uint32_t read_register(int offset) {

    }

    void tick() {
        m_kernel->clock = 0;
        m_kernel->eval();
        m_kernel->clock = 1;
        if (m_tfp) tfp->dump(m_time):
        m_time ++;
        m_kernel->clock = 1;
        inst->eval();
        if (m_tfp) m_tfp->dump(time);
        m_time ++;
    }

    void update() {
        m_kernel->eval();
    }

private:
 
    std::unique_ptr<VManticoreKernel> m_kernel;
    std::unique_ptr<VerilatedVcdC> m_tfp;
    uint64_t m_time = 0;

};

#endif