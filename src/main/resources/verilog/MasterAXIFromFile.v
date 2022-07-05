`timescale 1 ns / 1 ps

	module MasterAXIFromFile #
	(
		// Base address of targeted slave
		parameter  C_M_TARGET_SLAVE_BASE_ADDR	= 48'h000000000000,
		// Burst Length. Supports 1, 2, 4, 8, 16, 32, 64, 128, 256 burst lengths
		// Set to 1
		parameter integer C_M_AXI_BURST_LEN	= 16,
		// Thread ID Width
		// Set to 0
		parameter integer C_M_AXI_ID_WIDTH	= 1,
		// Width of Address Bus
		parameter integer C_M_AXI_ADDR_WIDTH	= 32,
		// Width of Data Bus 
		// 2 in binary is 3'b010 which corresponds to 4 byte Data Width 
		parameter integer C_M_AXI_DATA_WIDTH	= 32,
		// Width of User Write Address Bus
		parameter integer C_M_AXI_AWUSER_WIDTH	= 0,
		// Width of User Read Address Bus
		parameter integer C_M_AXI_ARUSER_WIDTH	= 0,
		// Width of User Write Data Bus
		parameter integer C_M_AXI_WUSER_WIDTH	= 0,
		// Width of User Read Data Bus
		parameter integer C_M_AXI_RUSER_WIDTH	= 0,
		// Width of User Response Bus
		parameter integer C_M_AXI_BUSER_WIDTH	= 0
	)
	(
		// Initiate AXI transactions
		//input wire [C_M_AXI_DATA_WIDTH-1 : 0] cache_data,
		
		input wire  read_txn_start,
		input wire  write_txn_start,
        input wire [C_M_AXI_DATA_WIDTH-1:0] data_in,
        output reg [C_M_AXI_DATA_WIDTH-1:0] data_out,
        input wire [7:0] burst_size,
        input wire [C_M_AXI_ADDR_WIDTH-1 : 0] read_addr,
        input wire [C_M_AXI_ADDR_WIDTH-1 : 0] write_addr,
        input wire cache_ready,
		// Asserts when transaction is complete
		output reg read_txn_done,
		output reg write_txn_done,
		// Asserts when ERROR is detected
		output reg  ERROR,
		// Global Clock Signal.
		input wire  M_AXI_ACLK,
		// Global Reset Singal. This Signal is Active Low
		input wire  M_AXI_ARESETN,
		// Master Interface Write Address ID
		output wire [1'b1<<C_M_AXI_ID_WIDTH-1 : 0] M_AXI_AWID,
		// Master Interface Write Address
		output wire [C_M_AXI_ADDR_WIDTH-1 : 0] M_AXI_AWADDR,
		// Burst length. The burst length gives the exact number of transfers in a burst
		output reg [7 : 0] M_AXI_AWLEN,

		// Burst size. This signal indicates the size of each transfer in the burst
		output wire [2 : 0] M_AXI_AWSIZE,
		// Burst type. The burst type and the size information, 
    // determine how the address for each transfer within the burst is calculated.
		output wire [1 : 0] M_AXI_AWBURST,
		// Lock type. Provides additional information about the
    // atomic characteristics of the transfer.
		output wire  M_AXI_AWLOCK,
		// Memory type. This signal indicates how transactions
    // are required to progress through a system.
		output wire [3 : 0] M_AXI_AWCACHE,
		// Protection type. This signal indicates the privilege
    // and security level of the transaction, and whether
    // the transaction is a data access or an instruction access.
		output wire [2 : 0] M_AXI_AWPROT,
		// Quality of Service, QoS identifier sent for each write transaction.
		output wire [3 : 0] M_AXI_AWQOS,
		// Optional User-defined signal in the write address channel.
		output wire [C_M_AXI_AWUSER_WIDTH-1 : 0] M_AXI_AWUSER,
		// Write address valid. This signal indicates that
    // the channel is signaling valid write address and control information.
		output wire  M_AXI_AWVALID,
		// Write address ready. This signal indicates that
    // the slave is ready to accept an address and associated control signals
		input wire  M_AXI_AWREADY,
		// Master Interface Write Data.
		output wire [C_M_AXI_DATA_WIDTH-1 : 0] M_AXI_WDATA,
		// Write strobes. This signal indicates which byte
    // lanes hold valid data. There is one write strobe
    // bit for each eight bits of the write data bus.
		output wire [C_M_AXI_DATA_WIDTH/8-1 : 0] M_AXI_WSTRB,
		// Write last. This signal indicates the last transfer in a write burst.
		output wire  M_AXI_WLAST,
		// Optional User-defined signal in the write data channel.
		output wire [C_M_AXI_WUSER_WIDTH-1 : 0] M_AXI_WUSER,
		// Write valid. This signal indicates that valid write
    // data and strobes are available
		output wire  M_AXI_WVALID,
		// Write ready. This signal indicates that the slave
    // can accept the write data.
		input wire  M_AXI_WREADY,
		// Master Interface Write Response.
		input wire [C_M_AXI_ID_WIDTH-1 : 0] M_AXI_BID,
		// Write response. This signal indicates the status of the write transaction.
		input wire [1 : 0] M_AXI_BRESP,
		// Optional User-defined signal in the write response channel
		input wire [C_M_AXI_BUSER_WIDTH-1 : 0] M_AXI_BUSER,
		// Write response valid. This signal indicates that the
    // channel is signaling a valid write response.
		input wire  M_AXI_BVALID,
		// Response ready. This signal indicates that the master
    // can accept a write response.
		output wire  M_AXI_BREADY,
		// Master Interface Read Address.
		output wire [C_M_AXI_ID_WIDTH-1 : 0] M_AXI_ARID,
		// Read address. This signal indicates the initial
    // address of a read burst transaction.
		output wire [C_M_AXI_ADDR_WIDTH-1 : 0] M_AXI_ARADDR,
		// Burst length. The burst length gives the exact number of transfers in a burst
		output reg [7 : 0] M_AXI_ARLEN,
		// Burst size. This signal indicates the size of each transfer in the burst
		output wire [2 : 0] M_AXI_ARSIZE,
		// Burst type. The burst type and the size information, 
    // determine how the address for each transfer within the burst is calculated.
		output wire [1 : 0] M_AXI_ARBURST,
		// Lock type. Provides additional information about the
    // atomic characteristics of the transfer.
		output wire  M_AXI_ARLOCK,
		// Memory type. This signal indicates how transactions
    // are required to progress through a system.
		output wire [3 : 0] M_AXI_ARCACHE,
		// Protection type. This signal indicates the privilege
    // and security level of the transaction, and whether
    // the transaction is a data access or an instruction access.
		output wire [2 : 0] M_AXI_ARPROT,
		// Quality of Service, QoS identifier sent for each read transaction
		output wire [3 : 0] M_AXI_ARQOS,
		// Optional User-defined signal in the read address channel.
		output wire [C_M_AXI_ARUSER_WIDTH-1 : 0] M_AXI_ARUSER,
		// Write address valid. This signal indicates that
    // the channel is signaling valid read address and control information
		output wire  M_AXI_ARVALID,
		// Read address ready. This signal indicates that
    // the slave is ready to accept an address and associated control signals
		input wire  M_AXI_ARREADY,
		// Read ID tag. This signal is the identification tag
    // for the read data group of signals generated by the slave.
		input wire [C_M_AXI_ID_WIDTH-1 : 0] M_AXI_RID,
		// Master Read Data
		input wire [C_M_AXI_DATA_WIDTH-1 : 0] M_AXI_RDATA,
		// Read response. This signal indicates the status of the read transfer
		input wire [1 : 0] M_AXI_RRESP,
		// Read last. This signal indicates the last transfer in a read burst
		input wire  M_AXI_RLAST,
		// Optional User-defined signal in the read address channel.
		input wire [C_M_AXI_RUSER_WIDTH-1 : 0] M_AXI_RUSER,
		// Read valid. This signal indicates that the channel
    // is signaling the required read data.
		input wire  M_AXI_RVALID,
		// Read ready. This signal indicates that the master can
    // accept the read data and response information.
		output wire  M_AXI_RREADY
		
	);


	// Burst length for transactions, in C_M_AXI_DATA_WIDTHs.
	// Non-2^n lengths will eventually cause bursts across 4K address boundaries.
	//localparam integer C_MASTER_LENGTH	= 12;
	// total number of burst transfers is master length divided by burst length and burst size
	//localparam integer C_NO_BURSTS_REQ = C_MASTER_LENGTH-clogb2((C_M_AXI_BURST_LEN*C_M_AXI_DATA_WIDTH/8)-1);
	// Example State machine to initialize counter, initialize write transactions, 
	// initialize read transactions and comparison of read data with the 
	// written data words.
	reg [7 : 0] M_AXI_AWLEN_buf;

	// AXI4LITE signals
	//AXI4 internal temp signals
	reg [C_M_AXI_ADDR_WIDTH-1 : 0] 	axi_awaddr;
	reg [C_M_AXI_ADDR_WIDTH-1 : 0] 	axi_data;
	
	reg  	axi_awvalid;
	reg [C_M_AXI_DATA_WIDTH-1 : 0] 	axi_wdata;
	reg [C_M_AXI_DATA_WIDTH-1 : 0] 	axi_wdata_buffer;
	reg  	axi_wlast;
	reg  	axi_wvalid;
	reg  	axi_bready;
	reg [C_M_AXI_ADDR_WIDTH-1 : 0] 	axi_araddr;
	reg  	axi_arvalid;
	reg  	axi_rready;
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
	reg  	start_single_burst_write;
	reg  	start_single_burst_read;
	// reg     write_txn_done;
	// reg  	read_txn_done;
	reg  	error_reg;
	reg  	compare_done;
	reg  	read_mismatch;
	reg  	burst_write_active;
	reg  	burst_read_active;
	reg [C_M_AXI_DATA_WIDTH-1 : 0] 	expected_rdata;
	//Interface response error flags
	wire  	write_resp_error;
	wire  	read_resp_error;
	wire  	wnext;
	wire  	rnext;
	reg  	init_txn_ff;
    reg     temp=0;
    reg [1:0]read_state;
    reg [1:0]write_state;
	reg [7:0]burst_size_temp;

	case (C_M_AXI_DATA_WIDTH)
	8:
	begin
        assign M_AXI_ARSIZE	= 3'b000;
        assign M_AXI_AWSIZE	= 3'b000;    
	end
	16:
	begin
	    assign M_AXI_ARSIZE	= 3'b001;
        assign M_AXI_AWSIZE	= 3'b001; 
	end
	32:
	begin
	    assign M_AXI_ARSIZE	= 3'b010;
        assign M_AXI_AWSIZE	= 3'b010; 
	end
	64:
	begin
	    assign M_AXI_ARSIZE	= 3'b011;
        assign M_AXI_AWSIZE	= 3'b011; 
	end
	128:
	begin
	    assign M_AXI_ARSIZE	= 3'b100;
        assign M_AXI_AWSIZE	= 3'b100; 
	end
	256:
	begin
	    assign M_AXI_ARSIZE	= 3'b101;
        assign M_AXI_AWSIZE	= 3'b101; 
	end
	512:
	begin
	    assign M_AXI_ARSIZE	= 3'b110;
        assign M_AXI_AWSIZE	= 3'b110; 
	end
	1024:
	begin
	    assign M_AXI_ARSIZE	= 3'b111;
        assign M_AXI_AWSIZE	= 3'b111; 
	end

	endcase	

	// I/O Connections assignments
    //assign slave_ready = M_AXI_WREADY;
	//I/O Connections. Write Address (AW)
	assign M_AXI_AWID	= 'b0;
	//The AXI address is a concatenation of the target base address + active offset range
	assign M_AXI_AWADDR	= C_M_TARGET_SLAVE_BASE_ADDR + axi_awaddr;
	//Burst LENgth is number of transaction beats, minus 1
	//Size should be C_M_AXI_DATA_WIDTH, in 2^SIZE bytes, otherwise narrow bursts are used
	
	//INCR burst type is usually used, except for keyhole bursts
	assign M_AXI_AWBURST	= 2'b01;
	assign M_AXI_AWLOCK	= 1'b0;
	//Update value to 4'b0011 if coherent accesses to be used via the Zynq ACP port. Not Allocated, Modifiable, not Bufferable. Not Bufferable since this example is meant to test memory, not intermediate cache. 
	assign M_AXI_AWCACHE	= 4'b0010;
	assign M_AXI_AWPROT	= 3'h0;
	assign M_AXI_AWQOS	= 4'h0;
	assign M_AXI_AWUSER	= 'b1;
	assign M_AXI_AWVALID	= axi_awvalid;
	//Write Data(W)
	assign M_AXI_WDATA	= axi_wdata;
	//All bursts are complete and aligned in this example
	assign M_AXI_WSTRB	= {(C_M_AXI_DATA_WIDTH/8){1'b1}};
	assign M_AXI_WLAST	= axi_wlast;
	assign M_AXI_WUSER	= 'b0;
	assign M_AXI_WVALID	= axi_wvalid;
	//Write Response (B)
	assign M_AXI_BREADY	= axi_bready;
	//Read Address (AR)
	assign M_AXI_ARID	= 'b0;
	assign M_AXI_ARADDR	= C_M_TARGET_SLAVE_BASE_ADDR + axi_araddr;

	//Size should be C_M_AXI_DATA_WIDTH, in 2^n bytes, otherwise narrow bursts are used
	//INCR burst type is usually used, except for keyhole bursts
	assign M_AXI_ARBURST	= 2'b01;
	assign M_AXI_ARLOCK	= 1'b0;
	//Update value to 4'b0011 if coherent accesses to be used via the Zynq ACP port. Not Allocated, Modifiable, not Bufferable. Not Bufferable since this example is meant to test memory, not intermediate cache. 
	assign M_AXI_ARCACHE	= 4'b0010;
	assign M_AXI_ARPROT	= 3'h0;
	assign M_AXI_ARQOS	= 4'h0;
	assign M_AXI_ARUSER	= 'b1;
	assign M_AXI_ARVALID	= axi_arvalid;
	//Read and Read Response (R)
	assign M_AXI_RREADY	= axi_rready;
	//Example design I/O
	integer counter = 0;
	//Burst size in bytes
	//assign burst_size_bytes	= C_M_AXI_BURST_LEN * C_M_AXI_DATA_WIDTH/8;

    always @(posedge M_AXI_ACLK)
    begin  
		if (M_AXI_ARESETN == 0)
        begin
            axi_wvalid <= 0;
            axi_awvalid <= 0;
            axi_bready <= 0;
            write_state <= 0;
            axi_wlast <= 0;
            write_txn_done <= 0;		    
        end 
		else
		begin
			axi_bready <= 1;
			case(write_state)
			2'b00:
			begin
				write_txn_done <= 0;
				if(write_txn_start==1)
				begin
					axi_awvalid <= 1;
					axi_awaddr <= write_addr;
					//axi_wdata_buffer <= data_in;
					M_AXI_AWLEN	<= burst_size;
					burst_size_temp <= burst_size;
					write_state <= 1;
					counter <= 0;
				end
			end
			2'b01:
			begin
					axi_awvalid <= 0;
					//axi_wdata_buffer <= data_in;
					axi_wdata <= data_in;    
					axi_wvalid <= 1;
					counter <= counter + 1;
					if (counter == burst_size_temp-1)
					begin
						axi_wlast <= 1;
						write_state <= 2;
					end       
			end
			2'b10:
			begin
				if (M_AXI_WREADY == 1)
				begin
					axi_wlast <= 0;
					axi_wvalid <=0;
					write_txn_done <= 1;
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


	always @(posedge M_AXI_ACLK)
	begin
		if (M_AXI_ARESETN == 0)
        begin
            read_txn_done <= 0;
            axi_arvalid <= 0;
			axi_rready <= 0;
		    read_state <= 0;
        end 
		else
		begin
			case(read_state)
				2'b00:
				begin 
					read_txn_done <= 0;
					if(read_txn_start==1)
					begin
						axi_araddr <= read_addr;
						M_AXI_ARLEN	<= 0;
						axi_arvalid <= 1;
						read_state <= 1;
					end
				end        
				2'b01:
				begin
					if(M_AXI_ARREADY==1)
					begin
						axi_arvalid <= 0;
						axi_rready <= 1;
						read_state <= 2;
					end
				end
				2'b10:
				begin
					if (M_AXI_RLAST == 1 & M_AXI_RVALID == 1)
					begin
						data_out <= M_AXI_RDATA;
						read_txn_done <= 1;
						axi_rready <= 0;
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
endmodule
