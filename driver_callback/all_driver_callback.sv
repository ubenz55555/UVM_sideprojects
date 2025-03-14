
import uvm_pkg::*;
interface apb_if(input logic pclk);
	logic reset_n;
	logic psel;
	logic pready;
	logic penable;
	logic pwrite;
	logic [31:0] paddr;
	logic [31:0] pwdata;
	logic [31:0] prdata;
	//logic pslverr;
  
	clocking cbm @(posedge pclk);
		output psel;
		input pready;
		output penable; 
		output pwrite; 
		output paddr;
		output  pwdata;
		input  prdata;
	//	input pslverr;
	endclocking
  
  	//Monitor clocking block
  	clocking cb_Mon @(posedge pclk);
      	input psel;
		input pready;
      	input penable; 
		input pwrite; 
		input paddr;
		input  pwdata;
		input  prdata;
	endclocking  
	modport master(clocking cbm, output reset_n);
    modport Mon(clocking cb_Mon);
endinterface
      
typedef enum {READ,WRITE} mode_e;
 
class apb_trans extends uvm_sequence_item;
   
	rand logic [31:0] addr[];
	rand logic [31:0] wdata[]; 
	rand mode_e mode;
	rand int unsigned pkt_size;
  
    constraint values{
      foreach(addr[i]) {
       soft addr[i] == 4*i; 
    
      }
        foreach(wdata[i]) {
          //wdata[i] inside {[1000000000:1900000000]};
  		wdata[i] == 32'd16;
        }
    }
        
      constraint valid_size{
        addr.size == pkt_size;
        wdata.size == pkt_size;       
        soft pkt_size == 3;       
        solve pkt_size before addr, wdata;
      }
	 
	 `uvm_object_utils_begin(apb_trans)
	 `uvm_field_array_int(addr, UVM_ALL_ON)
	 `uvm_field_array_int(wdata, UVM_ALL_ON)
     `uvm_field_enum(mode_e,mode, UVM_ALL_ON)
     `uvm_field_int(pkt_size, UVM_ALL_ON);
	 `uvm_object_utils_end
	 function new(string name="apb_trans");
		 super.new(name);
	 endfunction
endclass

class apb_sequence extends uvm_sequence#(apb_trans);
	`uvm_object_utils(apb_sequence)
	
	int unsigned pkt_count;
	apb_trans ref_pkt;
  
	function new(string name="apb_sequence");
		super.new(name);
	endfunction
	virtual task pre_start();
		pkt_count=10;
	endtask
  
	logic [31:0] exp_addr[];
	logic [4:0] exp_size;
  
	task body();
	int unsigned pkt_id;
      
    req = apb_trans::type_id::create("pkt");
      
      assert(req.randomize() with {req.mode == WRITE;});
     // req.addr[0] = 'd0;  // To test the un allined addredd write
         start_item(req);
     	// req.print();
        finish_item(req);       
      
      assert(req.randomize() with {req.mode == READ;});
      // req.addr[0] = 'd2;
         start_item(req);
     	// req.print();
        finish_item(req);
    
      req.valid_size.constraint_mode(0);
      
      assert(req.randomize() with {req.mode == READ; req.pkt_size == 1; req.addr.size()==1; req.wdata.size()==1;req.addr[0] == 'd2;});
     // req.addr[0] = 'd2;  // To test the un allined addredd write
         start_item(req);
     	// req.print();
        finish_item(req);
      
           req.valid_size.constraint_mode(0);
      assert(req.randomize() with {req.mode == WRITE; req.pkt_size == 4; req.addr.size()==4; req.wdata.size()==4;req.addr[0] == 'd2;});
      //req.addr[0] = 'd2;  // To test the un allined addredd write
         start_item(req);
     	// req.print();
        finish_item(req);
      
       req.valid_size.constraint_mode(0);
      assert(req.randomize() with {req.mode == READ; req.pkt_size == 2; req.addr.size()==2; req.wdata.size()==2; req.addr[0] == 'd3;});
      //req.addr[0] = 'd3;  // To test the un allined addredd write
         start_item(req);
     	// req.print();
      finish_item(req);   
    endtask
  
endclass

class apbm_sequencer extends uvm_sequencer#(apb_trans);
	`uvm_component_utils(apbm_sequencer);
	function new(string name="apbm_sequencer",uvm_component parent);
		super.new(name,parent);
	endfunction
endclass
class apbm_driver extends uvm_driver#(apb_trans);
	`uvm_component_utils(apbm_driver)

	virtual apb_if vif;
	bit [31:0] pkt_id;
  
	function new(string name="apbm_driver",uvm_component parent);
		super.new(name,parent);
	endfunction

	extern virtual task run_phase(uvm_phase phase);
	//extern virtual task reset_phase(uvm_phase phase);
	extern virtual function void build_phase(uvm_phase phase);
	extern virtual task write(input logic [31:0] addr_local,wdata_local);
	extern virtual task read(input logic [31:0] addr_local);
    extern virtual task reset();
    extern virtual task reset_drvr();
	extern virtual task drive(input logic [31:0] addr_local,wdata_local);
      
      extern virtual task wdata_write(input logic [31:0] addr_local,wdata_local);
endclass

	function void apbm_driver::build_phase(uvm_phase phase);
		super.build_phase(phase);
		if(uvm_config_db#(virtual apb_if)::get(this,"","drvr_if",vif) == null) `uvm_fatal(get_type_name(),"Virtual interface in apb_driver is NULL")
	//	assert(vif != null) else
	//		`uvm_fatal(get_type_name(),"Virtual interface in apb_driver is NULL")
	endfunction
	
	task apbm_driver::run_phase(uvm_phase phase);
		super.run_phase(phase);
		reset_drvr();
	
		@(vif.cbm);
	
		forever begin
			seq_item_port.get_next_item(req);
		//	pkt_id++;
		//  `uvm_info(get_type_name(),$sformatf("[%0d] %s",pkt_id,req.convert2string()),UVM_MEDIUM)
		
		@(vif.cbm);
	
		for(int i=0; i<req.pkt_size;i++) begin
		
		drive(req.addr[i],req.wdata[i]);
	//  drive(req.addr,req.wdata);
		
			@(vif.cbm);
			vif.cbm.penable <= 0;
		end
		
		vif.cbm.psel <= 0;
		seq_item_port.item_done();		
		end
	endtask
	
	task apbm_driver::drive(input logic [31:0] addr_local,wdata_local);
			case(req.mode)
			//  RESET: reset();
			READ: read(addr_local);
			WRITE: write(addr_local,wdata_local);
			endcase
	endtask

  task apbm_driver::write(input logic [31:0] addr_local,wdata_local);
    `uvm_info("Write","APB Write transaction started...",UVM_HIGH)

		vif.cbm.psel 	<= 1;
		vif.cbm.pwrite 	<= 1;
		vif.cbm.paddr 	<= addr_local;
		wdata_write(addr_local, wdata_local);
    
		while(1) begin
		@(vif.cbm);
			vif.cbm.penable <=1;
			if(vif.pready == 1)  
				break;
		end  
	endtask

	task apbm_driver::read(input logic [31:0] addr_local);
		`uvm_info("Read","APB Read transaction started...",UVM_HIGH)
			vif.cbm.psel <= 1;
			vif.cbm.pwrite <= 0;
			// vif.cbm.penable <= 0;
			vif.cbm.paddr <= addr_local;
			
			while(1) begin
				@(vif.cbm);
				vif.cbm.penable <=1;
				if(vif.pready == 1)
				break;
				//@(vif.cbm);
			
			end
		`uvm_info("Read","APB Read transaction ended...",UVM_HIGH)
	endtask

	task apbm_driver::reset();
	
			`uvm_info("Driver","Applying reset to APB",UVM_HIGH)
			vif.cbm.psel <= 0;
			vif.cbm.penable <= 0;
			vif.reset_n <= 0;
			vif.cbm.paddr <= 0;
			vif.cbm.pwrite <=0;
		// vif.cbm.pwdata <= 0;
			@(vif.cbm);
			@(vif.cbm);
			vif.reset_n <= 1;
			vif.cbm.psel <= 0;
			vif.cbm.penable <= 0;
			@(vif.cbm);
			@(vif.cbm);
			`uvm_info("Driver","APB out of reset",UVM_HIGH)
			
	endtask

  task apbm_driver::reset_drvr();
		@(negedge vif.reset_n)
		
		vif.cbm.psel <= 0;
		vif.cbm.penable <= 0;
		vif.reset_n <= 0;
		vif.cbm.paddr <= 0;
		vif.cbm.pwrite <=0;
    
  endtask
 
  task apbm_driver::wdata_write(input logic [31:0] addr_local, wdata_local);

    case(addr_local%4)
       0: 	begin
         
      		vif.cbm.pwdata <= wdata_local;
       		end
       1: 	begin
         		vif.cbm.pwdata[15:8] <= wdata_local[15:8];
         		vif.cbm.pwdata[23:16] <= wdata_local[23:16];
         		vif.cbm.pwdata[31:24] <= wdata_local[31:24];
        
            end
       2: 	begin
         		vif.cbm.pwdata[23:16] <= wdata_local[23:16];
         		vif.cbm.pwdata[31:24] <= wdata_local[31:24];
       
      		end
       3: 	begin
         		vif.cbm.pwdata[31:24] <= wdata_local[31:24];

      		end
      default: begin
        		$display("[Inside] Default");
      	 	   end
    endcase
    
    endtask
  //
class apbm_monitor extends uvm_monitor;
  `uvm_component_utils(apbm_monitor);
  
  virtual apb_if.Mon vif;
  
  reg [31:0] prdata_q[$];
  reg [31:0] pwdata_q[$];
  
    reg [31:0] paddr_q[$];
 
  mode_e mode_q[$];
  
  reg [7:0] mem_mon[4];
  reg [31:0] mem;
  
  int transaction_pkt;
  
  int tr_no;
  
	function new(string name="apbm_monitor",uvm_component parent);
		super.new(name,parent);
	endfunction
  
	uvm_analysis_port #(apb_trans) analysis_port;
  
	protected apb_trans tr;
  
	extern virtual task run_phase(uvm_phase phase);
	extern virtual function void build_phase(uvm_phase phase);
	extern virtual function void connect_phase(uvm_phase phase);
    extern task send_transaction(input int pkt_size);
endclass
  
  function void apbm_monitor::build_phase(uvm_phase phase) ;
		super.build_phase(phase);
		if (!uvm_config_db#(virtual apb_if.Mon)::get(get_parent(), "", "Mon_if", vif)) begin
		`uvm_fatal("CFGERR", "Monitor DUT interface not set");
		end

		analysis_port=new("analysis_port",this);
	endfunction
  
     function void apbm_monitor::connect_phase(uvm_phase phase) ;
       super.connect_phase(phase);
       tr = apb_trans::type_id::create("tr",this);
     endfunction
    
    
  task apbm_monitor::run_phase(uvm_phase phase);
		forever begin
		
		if(~vif.cb_Mon.psel) begin
			send_transaction(transaction_pkt);
		end
		
		@(vif.cb_Mon);
		
		if(vif.cb_Mon.psel) begin // Check for transfer enable
			
			transaction_pkt++;// Size of the transaction
			
			if(vif.cb_Mon.pwrite) begin
				//pwaddr_q.push_back(vif.cb_Mon.paddr);
			mode_q.push_back(WRITE);
			end
			else begin
				//praddr_q.push_back(vif.cb_Mon.paddr);
			mode_q.push_back(READ);
			end
			
			paddr_q.push_back(vif.cb_Mon.paddr);
			
			while(1) begin
			
			@(vif.cb_Mon);
			
			if(vif.cb_Mon.penable && vif.cb_Mon.pready) begin			
				if(vif.cb_Mon.pwrite) begin				
					pwdata_q.push_back(vif.cb_Mon.pwdata);
				
				end				
				else begin
					prdata_q.push_back(vif.cb_Mon.prdata);				
				end				
				break;
			end			
			end			
			end
		end
  endtask
  
  task apbm_monitor::send_transaction(input int pkt_size);
    if(pkt_size > 0) begin

      tr.pkt_size=pkt_size;
      tr.addr=new[pkt_size];
      tr.wdata=new[pkt_size];
      tr_no++;
      
      
      foreach(mode_q[i]) begin
      // Write address and data pop out
        if(mode_q[i] == WRITE) begin
       // for(int i=0;i<pkt_size;i++ ) begin
          tr.addr[i] = paddr_q.pop_front();
          tr.wdata[i] = pwdata_q.pop_front();
			tr.mode = WRITE;
          // end
      end
      // Read address and data pop out
      else begin
       // for(int i=0;i<pkt_size;i++ ) begin
          tr.addr[i] = paddr_q.pop_front();
          tr.wdata[i] = prdata_q.pop_front();
        tr.mode = READ;
       // end
      end
      end// Foreach
      $display("[Send_pkt] [%0s] paddr=%0p :: pdata=%0p",tr.mode,tr.addr,tr.wdata);      
     // pwaddr_q.delete();
      pwdata_q.delete();
     // praddr_q.delete();
      prdata_q.delete();
      
      paddr_q.delete();
      mode_q.delete();
      transaction_pkt=0;    
      //Broadcasting
      analysis_port.write(tr);   
    end
  endtask

class apb_master_agent extends uvm_agent;
	`uvm_component_utils(apb_master_agent)

	apbm_sequencer apbm_seqr;
	apbm_driver apbm_drv;
	apbm_monitor  apbm_mon;	
  
	uvm_analysis_port#(apb_trans) ap;

	function new(string name="apb_master_agent",uvm_component parent);
		super.new(name,parent);
	endfunction

	extern virtual function void build_phase(uvm_phase phase);
	extern virtual function void connect_phase(uvm_phase phase);
endclass

function void apb_master_agent::build_phase(uvm_phase phase);
	super.build_phase(phase);
  `uvm_info("[m_agent]","Start",UVM_HIGH)
	ap=new("ap",this);
	if(is_active == UVM_ACTIVE) begin
		apbm_seqr=apbm_sequencer::type_id::create("apbm_seqr",this);
		apbm_drv=apbm_driver::type_id::create("apbm_drv",this);
	end
  apbm_mon=apbm_monitor::type_id::create("apbm_mon",this);
  `uvm_info("[m_agent]","END",UVM_HIGH)
endfunction

function void apb_master_agent::connect_phase(uvm_phase phase);
	super.connect_phase(phase);
	if(is_active == UVM_ACTIVE) begin
		apbm_drv.seq_item_port.connect(apbm_seqr.seq_item_export);
	end
  		apbm_mon.analysis_port.connect(ap);
endfunction

//


class apb_scoreboard extends uvm_scoreboard;
  `uvm_component_utils(apb_scoreboard)
  
  uvm_analysis_imp #(apb_trans,apb_scoreboard) mon;
  
  // Ref Mem
  bit [7:0] mem[*];
  logic [31:0] prdata;
  
  bit [31:0] matched, mis_matched;

  
function new(string name="apb_scoreboard",uvm_component parent=null);
    super.new(name,parent);
endfunction
  
virtual function void build_phase(uvm_phase phase);
	super.build_phase(phase);
  `uvm_info("[apb_scoreboard]","Build Phase Start",UVM_HIGH);
		mon=new("mon",this);
  `uvm_info("[apb_scoreboard]","Build Phase Stop",UVM_HIGH);
endfunction

  virtual function write(apb_trans tr);
    
    $display("[Receive_pkt] [%0s] paddr=%0p :: pdata=%0p size=%0d",tr.mode,tr.addr,tr.wdata,tr.pkt_size);
    
    if(tr.mode == WRITE) begin
      write_to_mem(tr);

    end
    
    else if(tr.mode == READ) begin
      read_from_mem(tr);

    end
    else begin
      `uvm_error("[apb_scoreboard]","Invalid mode");
    end
 
  endfunction
  
  function void write_to_mem(apb_trans tr1);
    for(int i=0;i<tr1.pkt_size;i++) begin

      write_mem(tr1.addr[i],tr1.wdata[i]);
    end
  endfunction
  
  function void read_from_mem(apb_trans tr1);

     	for(int i=0;i<tr1.pkt_size;i++) begin
          read_mem(tr1.addr[i],tr1.wdata[i]);
          $display("[Prdata]=%h",prdata);
          if(prdata === tr1.wdata[i]) begin       
            matched++;
          end
          else begin
            mis_matched++;
          end
        end//end_of_for

  endfunction
  
  virtual function void final_phase(uvm_phase phase);
    $display("mem=%p",mem); 
    $display("Matched=%0d Mis_Matched=%0d",matched,mis_matched); 
  endfunction
  
  
  //*************************************mem_logic************************
  
  function void write_mem(input logic [31:0] paddr,pwdata);
    case(paddr%4)
       0: 	begin
         
         		mem[paddr] <= pwdata[7:0];
        	    mem[paddr+1] <= pwdata[15:8];
         	    mem[paddr+2] <= pwdata[23:16];
        	    mem[paddr+3] <= pwdata[31:24];
      	
       		end
       1: 	begin
         		mem[paddr] <= pwdata[15:8];
         		mem[paddr+1] <= pwdata[23:16];
        		mem[paddr+2] <= pwdata[31:24];
        
            end
       2: 	begin
       			mem[paddr] <= pwdata[23:16];
         		mem[paddr+1] <= pwdata[31:24];
       
      		end
       3: 	begin
         		mem[paddr] <= pwdata[31:24];

      		end
      default: begin
        $display("[Inside Write_mem] Default Addr=%0h",paddr);
      	 	   end
    endcase
  endfunction
  
    function void read_mem(input logic [31:0] paddr,pwdata);
    
    case(paddr%4)
      0: begin
  		 prdata[7:0] = mem[paddr];
         prdata[15:8] = mem[paddr+1];
         prdata[23:16] = mem[paddr+2];
         prdata[31:24] = mem[paddr+3];
      end
      1: begin
        prdata[7:0] = mem[paddr-1];
        prdata[15:8] = mem[paddr];
        prdata[23:16] = mem[paddr+1];
        prdata[31:24] = mem[paddr+2];
      end
      2: begin
        prdata[7:0] = mem[paddr-2];
        prdata[15:8] = mem[paddr-1];
         prdata[23:16] = mem[paddr];
        prdata[31:24] = mem[paddr+1];
      end
      3: begin
        prdata[7:0] = mem[paddr-3];
        prdata[15:8] = mem[paddr-2];
        prdata[23:16] = mem[paddr-1];
         prdata[31:24] = mem[paddr];
      end
      default: begin
        $display("[Inside Read_mem] Default Addr=%0h",paddr);
      end
    endcase
    endfunction
  
endclass

class apb_env extends uvm_env;
	`uvm_component_utils(apb_env)

	apb_master_agent m_agent;
  	apb_scoreboard   apb_scb;
	
	function new(string name="apb_env",uvm_component parent);
		super.new(name,parent);
	endfunction

	extern virtual function void build_phase(uvm_phase phase);
      extern virtual function void connect_phase(uvm_phase phase);
endclass

function void apb_env::build_phase(uvm_phase phase);
	super.build_phase(phase);
	m_agent=apb_master_agent::type_id::create("m_agent",this);
    apb_scb=apb_scoreboard::type_id::create("apb_scb",this);
endfunction

function void apb_env::connect_phase(uvm_phase phase);
    m_agent.ap.connect(apb_scb.mon);
endfunction
      
       
 program automatic pgm_test(apb_if pif);
	class apb_test extends uvm_test;
	`uvm_component_utils(apb_test)

	bit [31:0] pkt_count;

	virtual apb_if.master drvr_vif;

	apb_env env;
  apb_sequence apb_seq;

	function new(string name="apb_test",uvm_component parent);
		super.new(name,parent);
	endfunction
	extern virtual function void build_phase(uvm_phase phase);
      extern virtual  task run_phase(uvm_phase phase);

      
      virtual function void end_of_elaboration_phase(uvm_phase phase);
        print();
      endfunction
      
endclass

	function void apb_test::build_phase(uvm_phase phase);
		super.build_phase(phase);
		`uvm_info("[apb_test]","Build Phase Start",UVM_HIGH);
			pkt_count=10;
		`uvm_info("Test",$sformatf("Pkt_count = %0d",pkt_count),UVM_HIGH);
			uvm_config_db#(int)::set(this,"env.m_agent.apbm_seqr","item_count",pkt_count);
		apb_seq=apb_sequence::type_id::create("seq_pkt",this);
		
		env=apb_env::type_id::create("apb_env",this);
		
		`uvm_info("[apb_test]",$sformatf("Build Phase End"),UVM_HIGH)
	endfunction
      
        task apb_test::run_phase(uvm_phase phase);
				super.run_phase(phase);				
				phase.phase_done.set_drain_time(this,10);				
				`uvm_info("Apb_Test","Applying run phase to APB",UVM_HIGH)
				phase.raise_objection(this);					
				`uvm_info("APb_Test","Out of phase objection",UVM_HIGH)											
				apb_seq.start(env.m_agent.apbm_seqr);			
				phase.drop_objection(this);
        endtask

	initial begin
			uvm_config_db#(virtual apb_if)::set(null,"*","drvr_if",pif);
			uvm_config_db#(virtual apb_if.Mon)::set(null,"*","Mon_if",pif.Mon);
			run_test();		
	end
endprogram   
        
	module top;
		bit clk=0;
		apb_if intf_apb(clk);
		
		apb_slave dut_slave(
		.paddr(intf_apb.paddr),
		.psel(intf_apb.psel),
		.penable(intf_apb.penable),
		.pwrite(intf_apb.pwrite),
		.prdata(intf_apb.prdata), 
		.pwdata(intf_apb.pwdata),
		.pclk(intf_apb.pclk),
		.prst(intf_apb.reset_n),
		.pready(intf_apb.pready)
		
		);
		
			pgm_test pgm_test_inst(intf_apb);
			always #5 clk = ~clk;
			
		initial begin
				$dumpvars();
				$dumpfile("apb.vcd");
				
				intf_apb.reset_n <=1;
				@(posedge clk);
				intf_apb.reset_n <=0;
				@(posedge clk);
				intf_apb.reset_n <=1;
		end
		
	endmodule
    
