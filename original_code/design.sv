
module apb_slave(
  output reg [31:0] prdata,
	output reg pready,
  input [31:0]paddr,pwdata,
	input psel,penable,pwrite,pclk,prst
	);
  

  
  reg [7:0] mem[255];
 // reg [31:0] mem[0:255];
  
  typedef enum{SETUP,WRITE,READ} state_e;

	state_e state;
  	
  always@(posedge pclk) begin
    pready<=$random;
  end
  
 always @(posedge pclk,negedge prst) begin
    if(~prst) begin
			pready <= 1;
      mem = '{255{'b0}};
     	// for(int i=0;i<256;i++) mem[i] <= 0;
    end
  
   else begin
			case(state)
				SETUP: begin
                  if(psel && (!penable)) begin
                    if(pwrite) begin
							state <= WRITE;
                      	
						end
						else begin
                         // prdata <= mem[paddr];
                          read();// Read task	
                          
							state <= READ;
						end
					end
				end
				WRITE: begin
                  if(pwrite && psel && penable) begin
                  //  mem[paddr] <= pwdata;
                    write(); // Write task
                    
					end
					state <= SETUP;
				end
				READ: begin
					state <= SETUP;
				end
              default: begin state <= SETUP; 
              end
			endcase
		end
  
 end

    task write();
    case(paddr%4)
       0: 	begin
         
         mem[paddr] <= pwdata[7:0];//+1; // Bug
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
        		$display("[Inside] Default");
      	 	   end
    endcase
  endtask
  
  task read();
    
    case(paddr%4)
      0: begin
  		 prdata[7:0] <= mem[paddr];
         prdata[15:8] <= mem[paddr+1];
         prdata[23:16] <= mem[paddr+2];
         prdata[31:24] <= mem[paddr+3];
      end
      1: begin
        prdata[7:0] <= mem[paddr-1];
        prdata[15:8] <= mem[paddr];
        prdata[23:16] <= mem[paddr+1];
        prdata[31:24] <= mem[paddr+2];
      end
      2: begin
        prdata[7:0] <= mem[paddr-2];
        prdata[15:8] <= mem[paddr-1];
         prdata[23:16] <= mem[paddr];
        prdata[31:24] <= mem[paddr+1];
      end
      3: begin
        prdata[7:0] <= mem[paddr-3];
        prdata[15:8] <= mem[paddr-2];
        prdata[23:16] <= mem[paddr-1];
         prdata[31:24] <= mem[paddr];
      end
      default: begin
         $display("[Inside] Default");
      end
    endcase
  endtask
 
endmodule
