`timescale 1 ns / 1 ps

    function integer clogb2;
    input [31:0] datain;
    integer i;
    begin
        clogb2 = 0;
        for(i = 0; 2**i < datain; i = i + 1)
        clogb2 = i + 1;
    end
    endfunction

	module axislave_vip #
	(
		parameter integer C_S_AXI_BURST_LEN	= 0,
		parameter integer MEM_SIZE	= 0,  //in bytes
		// Thread ID Width
		// Set to 0
		parameter integer CACHE_DATA_WIDTH = 16,
		parameter integer C_S_AXI_ID_WIDTH	= 1,
		// Width of Address Bus
		parameter integer C_S_AXI_ADDR_WIDTH	= 32,
		// Width of Data Bus 
		// 2 in binary is 3'b010 which corresponds to 4 byte Data Width 
		parameter integer C_S_AXI_DATA_WIDTH	= 16,
		// Width of User Write Address Bus
		parameter integer C_S_AXI_AWUSER_WIDTH	= 0,
		// Width of User Read Address Bus
		parameter integer C_S_AXI_ARUSER_WIDTH	= 0,
		// Width of User Write Data Bus
		parameter integer C_S_AXI_WUSER_WIDTH	= 0,
		// Width of User Read Data Bus
		parameter integer C_S_AXI_RUSER_WIDTH	= 0,
		// Width of User Response Bus
		parameter integer C_S_AXI_BUSER_WIDTH	= 0
	)
	(
		// Initiate AXI transactions
		//input wire [C_M_AXI_DATA_WIDTH-1 : 0] cache_data,
		input wire  read_txn_start,
		input wire  mem_wen,
		input wire [CACHE_DATA_WIDTH-1:0]mem_data_in,
		output reg [CACHE_DATA_WIDTH-1:0]mem_data_out,
		input wire [clogb2(MEM_SIZE)-1:0]mem_raddr,
		input wire [clogb2(MEM_SIZE)-1:0]mem_waddr,
        input wire cache_ready,
		input wire lock,
		output reg  ERROR,
		// Global Clock Signal.
		input wire  S_AXI_ACLK,
		// Global Reset Singal. This Signal is Active Low
		input wire  S_AXI_ARESETN,
		// Master Interface Write Address ID
		input wire [1'b1<<C_S_AXI_ID_WIDTH-1 : 0] S_AXI_AWID,
		// Master Interface Write Address
		input wire [C_S_AXI_ADDR_WIDTH-1 : 0] S_AXI_AWADDR,
		// Burst length. The burst length gives the exact number of transfers in a burst
		input wire [7 : 0] S_AXI_AWLEN,

		// Burst size. This signal indicates the size of each transfer in the burst
		input wire [2 : 0] S_AXI_AWSIZE,
		// Burst type. The burst type and the size information, 
    // determine how the address for each transfer within the burst is calculated.
		input wire [1 : 0] S_AXI_AWBURST,
		// Lock type. Provides additional information about the
    // atomic characteristics of the transfer.
		input wire  S_AXI_AWLOCK,
		// Memory type. This signal indicates how transactions
    // are required to progress through a system.
		input wire [3 : 0] S_AXI_AWCACHE,
		// Protection type. This signal indicates the privilege
    // and security level of the transaction, and whether
    // the transaction is a data access or an instruction access.
		input wire [2 : 0] S_AXI_AWPROT,
		// Quality of Service, QoS identifier sent for each write transaction.
		input wire [3 : 0] S_AXI_AWQOS,
		// Optional User-defined signal in the write address channel.
		input wire [C_S_AXI_AWUSER_WIDTH-1 : 0] S_AXI_AWUSER,
		// Write address valid. This signal indicates that
    // the channel is signaling valid write address and control information.
		input wire  S_AXI_AWVALID,
		// Write address ready. This signal indicates that
    // the slave is ready to accept an address and associated control signals
		output wire  S_AXI_AWREADY,
		// Master Interface Write Data.
		input wire [C_S_AXI_DATA_WIDTH-1 : 0] S_AXI_WDATA,
		// Write strobes. This signal indicates which byte
    // lanes hold valid data. There is one write strobe
    // bit for each eight bits of the write data bus.
		input wire [C_S_AXI_DATA_WIDTH/8-1 : 0] S_AXI_WSTRB,
		// Write last. This signal indicates the last transfer in a write burst.
		input wire  S_AXI_WLAST,
		// Write valid. This signal indicates that valid write
    // data and strobes are available
		input wire  S_AXI_WVALID,
		// Write ready. This signal indicates that the slave
    // can accept the write data.
		output wire  S_AXI_WREADY,
		// Master Interface Write Response.
		output wire [C_S_AXI_ID_WIDTH-1 : 0] S_AXI_BID,
		// Write response. This signal indicates the status of the write transaction.
		output wire [1 : 0] S_AXI_BRESP,
		// Optional User-defined signal in the write response channel
		input wire [C_S_AXI_BUSER_WIDTH-1 : 0] S_AXI_BUSER,
		// Write response valid. This signal indicates that the
    // channel is signaling a valid write response.
		output wire  S_AXI_BVALID,
		// Response ready. This signal indicates that the master
    // can accept a write response.
		input wire  S_AXI_BREADY,
		// Master Interface Read Address.
		input wire [C_S_AXI_ID_WIDTH-1 : 0] S_AXI_ARID,
		// Read address. This signal indicates the initial
    // address of a read burst transaction.
		input wire [C_S_AXI_ADDR_WIDTH-1 : 0] S_AXI_ARADDR,
		// Burst length. The burst length gives the exact number of transfers in a burst
		input wire [7 : 0] S_AXI_ARLEN,
		// Burst size. This signal indicates the size of each transfer in the burst
		input wire [2 : 0] S_AXI_ARSIZE,
		// Burst type. The burst type and the size information, 
    // determine how the address for each transfer within the burst is calculated.
		input wire [1 : 0] S_AXI_ARBURST,
		// Lock type. Provides additional information about the
    // atomic characteristics of the transfer.
		input wire  S_AXI_ARLOCK,
		// Memory type. This signal indicates how transactions
    // are required to progress through a system.
		input wire [3 : 0] S_AXI_ARCACHE,
		// Protection type. This signal indicates the privilege
    // and security level of the transaction, and whether
    // the transaction is a data access or an instruction access.
		input wire [2 : 0] S_AXI_ARPROT,
		// Quality of Service, QoS identifier sent for each read transaction
		input wire [3 : 0] S_AXI_ARQOS,
		// Optional User-defined signal in the read address channel.
		input wire [C_S_AXI_ARUSER_WIDTH-1 : 0] S_AXI_ARUSER,
		// Write address valid. This signal indicates that
    // the channel is signaling valid read address and control information
		input wire  S_AXI_ARVALID,
		// Read address ready. This signal indicates that
    // the slave is ready to accept an address and associated control signals
		output wire  S_AXI_ARREADY,
		// Read ID tag. This signal is the identification tag
    // for the read data group of signals generated by the slave.
		output wire [C_S_AXI_ID_WIDTH-1 : 0] S_AXI_RID,
		// Master Read Data
		output wire [C_S_AXI_DATA_WIDTH-1 : 0] S_AXI_RDATA,
		// Read response. This signal indicates the status of the read transfer
		output wire [1 : 0] S_AXI_RRESP,
		// Read last. This signal indicates the last transfer in a read burst
		output wire  S_AXI_RLAST,
		// Optional User-defined signal in the read address channel.
		input wire [C_S_AXI_RUSER_WIDTH-1 : 0] S_AXI_RUSER,
		// Read valid. This signal indicates that the channel
    // is signaling the required read data.
		output wire  S_AXI_RVALID,
		// Read ready. This signal indicates that the master can
    // accept the read data and response information.
		input wire  S_AXI_RREADY
		
	);


	// Burst length for transactions, in C_M_AXI_DATA_WIDTHs.
	// Non-2^n lengths will eventually cause bursts across 4K address boundaries.
	//localparam integer C_MASTER_LENGTH	= 12;
	// total number of burst transfers is master length divided by burst length and burst size
	//localparam integer C_NO_BURSTS_REQ = C_MASTER_LENGTH-clogb2((C_M_AXI_BURST_LEN*C_M_AXI_DATA_WIDTH/8)-1);
	// Example State machine to initialize counter, initialize write transactions, 
	// initialize read transactions and comparison of read data with the 
	// written data words.
	
	reg [C_S_AXI_DATA_WIDTH-1 : 0] 	axi_rdata;
	reg [C_S_AXI_DATA_WIDTH-1 : 0]  write_data;
	reg [C_S_AXI_DATA_WIDTH-1 : 0] 	read_data;
	
	reg [7:0]write_burst_size;
	reg [7:0]read_burst_size;
	reg [9:0]write_bus_size;
	reg  	axi_rlast;
	reg  [1:0]	axi_bresp;
	reg  	axi_rvalid;
	reg  	axi_bvalid;
	reg  	axi_arready;
	reg  	axi_awready;
	reg  	axi_wready;
	reg start_read;
	//write beat count in a burst
	//reg [C_TRANSACTIONS_NUM : 0] 	write_index;
	//read beat count in a burst
	//reg [C_TRANSACTIONS_NUM : 0] 	read_index;
	//size of C_M_AXI_BURST_LEN length burst in bytes
	//wire [C_TRANSACTIONS_NUM+2 : 0] 	burst_size_bytes;
	//The burst counters are used to track the number of burst transfers of C_M_AXI_BURST_LEN burst length needed to transfer 2^C_MASTER_LENGTH bytes of data.
	//reg [C_NO_BURSTS_REQ : 0] 	write_burst_counter;
	//reg [C_NO_BURSTS_REQ : 0] 	read_burst_counter;
	
	reg     slave_write_txn_done;
	reg  	slave_read_txn_done;
	reg  	error_reg;
	reg  	compare_done;
    reg [C_S_AXI_ADDR_WIDTH-1 : 0] write_addr;
	reg  	init_txn_ff;
    reg     temp=0;
    reg [1:0]read_state;
    reg [1:0]write_state;
    reg write_resp_state;
    reg [1:0]axi_rresp;

    reg [CACHE_DATA_WIDTH:0] mem[MEM_SIZE:0];

	// case (C_S_AXI_DATA_WIDTH)
	// 8:
	// begin
    //     assign S_AXI_ARSIZE	= 3'b000;
    //     assign S_AXI_AWSIZE	= 3'b000;    
	// end
	// 16:
	// begin
	//     assign S_AXI_ARSIZE	= 3'b001;
    //     assign S_AXI_AWSIZE	= 3'b001; 
	// end
	// 32:
	// begin
	//     assign S_AXI_ARSIZE	= 3'b010;
    //     assign S_AXI_AWSIZE	= 3'b010; 
	// end
	// 64:
	// begin
	//     assign S_AXI_ARSIZE	= 3'b011;
    //     assign S_AXI_AWSIZE	= 3'b011; 
	// end
	// 128:
	// begin
	//     assign S_AXI_ARSIZE	= 3'b100;
    //     assign S_AXI_AWSIZE	= 3'b100; 
	// end
	// 256:
	// begin
	//     assign S_AXI_ARSIZE	= 3'b101;
    //     assign S_AXI_AWSIZE	= 3'b101; 
	// end
	// 512:
	// begin
	//     assign S_AXI_ARSIZE	= 3'b110;
    //     assign S_AXI_AWSIZE	= 3'b110; 
	// end
	// 1024:
	// begin
	//     assign S_AXI_ARSIZE	= 3'b111;
    //     assign S_AXI_AWSIZE	= 3'b111; 
	// end

	// endcase	
    integer i=0;
	
	//Burst LENgth is number of transaction beats, minus 1
	//Size should be C_M_AXI_DATA_WIDTH, in 2^SIZE bytes, otherwise narrow bursts are used
	

	assign S_AXI_AWREADY	= axi_awready;
	assign S_AXI_ARREADY	= axi_arready;
	assign S_AXI_WREADY	= axi_wready;
	//Write Data(W)
	assign S_AXI_RDATA	= axi_rdata;
	//All bursts are complete and aligned in this example
	assign S_AXI_RLAST	= axi_rlast;
	//Write Response (B)
	assign S_AXI_BVALID	= axi_bvalid;
	assign S_AXI_BRESP	= axi_bresp;
	assign S_AXI_RRESP	= axi_rresp;
	//Read Address (AR)

	//Size should be C_M_AXI_DATA_WIDTH, in 2^n bytes, otherwise narrow bursts are used
	//INCR burst type is usually used, except for keyhole bursts
	assign S_AXI_RVALID	= axi_rvalid;
	//Example design I/O
	
	//Burst size in bytes
	//assign burst_size_bytes	= C_M_AXI_BURST_LEN * C_M_AXI_DATA_WIDTH/8;

    always @(posedge S_AXI_ACLK)
    begin  
		if (S_AXI_ARESETN == 0)
        begin
            axi_wready <= 0;
            axi_awready <= 0;
            write_state <= 0;
            slave_write_txn_done <= 0;		    
        end 
		else if(lock==0)
		begin
			case(write_state)
			2'b00:
			begin
				axi_awready <= 1;
				axi_wready	<= 1;
			    slave_write_txn_done <= 0;		    

				if(S_AXI_AWVALID==1)
				begin
					write_addr <= S_AXI_AWADDR;
					write_burst_size <= S_AXI_AWLEN;
					write_bus_size <= S_AXI_AWSIZE;
					write_state <= 1;
				end
			end
			2'b01:
			begin
				axi_awready <= 0;
				if (S_AXI_WVALID == 1 & S_AXI_WLAST == 1)
				begin
				    for(i=0;i<C_S_AXI_DATA_WIDTH/CACHE_DATA_WIDTH;i=i+1)
				    begin
					   mem[write_addr+i] = S_AXI_WDATA[(CACHE_DATA_WIDTH*(i))+:CACHE_DATA_WIDTH];
					end
					axi_wready <= 0;
				    slave_write_txn_done <= 1;
					write_state <= 0;
				end        
			end
			default:
				begin
					write_state <= 0;
				end

			endcase
		end
	end
	
	always @(posedge S_AXI_ACLK)
    begin  
		if (S_AXI_ARESETN == 0)
        begin
            axi_bvalid <= 0;
            axi_bresp <= 0;	    
        end 
		else if(lock==0)
		begin
			case(write_resp_state)
			2'b0:
			begin
				if(slave_write_txn_done==1)
				begin
				    axi_bvalid <= 1;
				    axi_bresp <= 0;
				    write_resp_state <= 1;
				end
			end
			2'b1:
			begin
				if (S_AXI_BREADY == 1)
				begin
					axi_bvalid <= 0;
					write_state <= 0;
				end        
			end
			default:
				begin
					write_state <= 0;
				end
			endcase
		end
	end


	always @(posedge S_AXI_ACLK)
	begin
		if (S_AXI_ARESETN == 0)
        begin
            slave_read_txn_done <= 0;
            axi_arready <= 0;
			axi_rvalid <= 0;
			axi_rlast <= 0;
			axi_rresp <= 0;
		    read_state <= 0;
        end 
		else if(lock==0)
		begin
			case(read_state)
				2'b00:
				begin 
				    axi_arready <= 1;
					slave_read_txn_done <= 0;
					if(S_AXI_ARVALID==1)
					begin
					    axi_arready <= 0;
						for(i=0;i<C_S_AXI_DATA_WIDTH/CACHE_DATA_WIDTH;i=i+1)
                        begin
                           axi_rdata[(CACHE_DATA_WIDTH*i)+:(CACHE_DATA_WIDTH)] <= mem[S_AXI_ARADDR+i];            
                        end
                        axi_rvalid <= 1;
						axi_rlast <= 1;
						read_burst_size <= S_AXI_ARLEN;
						axi_arready <= 0;
						read_state <= 1;
					end
				end        
				2'b01:
				begin
					if(S_AXI_RREADY==1)
					begin
						axi_rvalid <= 0;
						axi_rlast <= 0;
						slave_read_txn_done <= 1;
						read_state <= 0;
					end
				end
				default:
				begin
					read_state <= 0;
				end 
			endcase
		end
    end
    always@(posedge S_AXI_ACLK)
    begin
        if(S_AXI_ARESETN == 0)
        begin
            for(i=0;i<MEM_SIZE;i=i+1)
            begin
                mem[i]<=0;
            end
        end
        else 
        begin
            if(mem_wen==1)
            begin
                mem[mem_waddr] <= mem_data_in;
            end
            mem_data_out <= mem[mem_raddr];
        end
    end
endmodule
