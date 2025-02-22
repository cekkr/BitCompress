// Dummy I/O module (for reference only)
module io_module (
    input  wire        clk,
    input  wire        rst,
    input  wire        data_in_valid,
    input  wire [7:0]  data_in,
    output reg         data_in_ready,
    output reg         data_out_valid,
    output reg  [7:0]  data_out,
    input  wire        data_out_ready
);

  // In a real I/O module, you would handle flow control, buffering,
  // and potentially error handling.  This is a VERY simplified placeholder.

  always @(posedge clk) begin
    if (rst) begin
      data_in_ready <= 1'b1; // Assume always ready initially
      data_out_valid <= 1'b0;
      data_out <= 8'b0;
    end else begin
      // Dummy behavior: Echo input to output with one cycle delay.
      data_in_ready <= 1'b1; // Always ready
      if (data_in_valid) begin
          if (data_out_ready) begin
              data_out_valid <= 1'b1;
              data_out <= data_in;
          end else begin
              data_out_valid <= 1'b0;
          end
      end else begin
        data_out_valid <= 1'b0;
      end
    end
  end

endmodule

// K-map cell module
module karnaugh_map_cell #(
    parameter integer CELL_INDEX = 0, // Unique index 0-15 for a 4-variable K-map
    parameter integer NUM_VARIABLES = 4
) (
    input  wire        clk,
    input  wire        rst,
    input  wire        write_enable,    // Cell-specific write enable
    input  wire        value_in,        // Input value (0 or 1) for this cell
    input  wire [3:0]  neighbor_values,  // Values from N, E, S, W neighbors
    input  wire [3:0]  neighbor_updates, // Signals when neighbors are UPDATED
    output reg         cell_value,       // Current cell value
    output reg  [3:0]  group_signals,    // Signals indicating group membership (to neighbors)
    output reg         is_prime_implicant,
    output reg         prime_implicant_valid, // Indicates if is_prime_implicant is valid
    output reg  [1:0]  group_size        // 00=none, 01=2, 10=4, 11=8 (simplified: up to 4)
);

    // Gray Code calculation (same as before)
    function [3:0] gray_encode;
        input [3:0] binary;
        gray_encode = binary ^ (binary >> 1);
    endfunction

    function [3:0] get_neighbor_index;
        input integer direction; // 0=N, 1=E, 2=S, 3=W
        input [3:0] current_index;
        reg [3:0] neighbor_index;
        reg [3:0] binary_index;
        reg [1:0] row, col;
        reg [1:0] neighbor_row, neighbor_col;

        begin
            binary_index = {current_index[3] ^ current_index[2], current_index[2] ^ current_index[1], current_index[1] ^ current_index[0], current_index[0]};

            row = binary_index[3:2];
            col = binary_index[1:0];

            case (direction)
                0: neighbor_row = (row == 0) ? 2'b11 : row - 1; // North
                1: neighbor_col = (col == 3) ? 2'b00 : col + 1; // East
                2: neighbor_row = (row == 3) ? 2'b00 : row + 1; // South
                3: neighbor_col = (col == 0) ? 2'b11 : col - 1; // West
                default: begin
                            neighbor_row = row;
                            neighbor_col = col;
                         end
            endcase

            if(direction == 0 || direction == 2)
                neighbor_col = col;
            else
                neighbor_row = row;

            neighbor_index = gray_encode({neighbor_row, neighbor_col});
            get_neighbor_index = neighbor_index;

        end
    endfunction

    // --- Main Logic ---
     always @(posedge clk or posedge rst) begin
        if (rst) begin
            cell_value <= 1'b0;
            group_size <= 2'b00;
            is_prime_implicant <= 1'b0;
            prime_implicant_valid <= 1'b0;
            group_signals <= 4'b0000;
        end else begin
            // 1. Handle Writes (highest priority)
            if (write_enable) begin
                cell_value <= value_in;
                prime_implicant_valid <= 1'b0; // Invalidate prime implicant status on write
                // We might also invalidate group_size here, depending on how aggressive
                // we want to be about re-evaluation.  For now, we'll re-evaluate groups
                // in the next step.
            end

            // 2. Continuous Group Detection (simplified to groups of 2 and 4)
            group_signals <= 4'b0000; // Reset
            group_size <= 2'b00;       // Default to no group

            if (cell_value) begin
                // Check for groups of 2
                reg [3:0] group_of_2_found;
                group_of_2_found = 4'b0000;

                if (neighbor_values[0]) begin group_signals[0] <= 1'b1; group_of_2_found[0] <=1'b1; end // North
                if (neighbor_values[1]) begin group_signals[1] <= 1'b1; group_of_2_found[1] <=1'b1;end // East
                if (neighbor_values[2]) begin group_signals[2] <= 1'b1; group_of_2_found[2] <=1'b1;end // South
                if (neighbor_values[3]) begin group_signals[3] <= 1'b1; group_of_2_found[3] <=1'b1;end // West

                // Check for groups of 4 (based on groups of 2)
                if (group_of_2_found[0] && group_of_2_found[2] &&  // N & S
                    neighbor_values[get_neighbor_index(0, CELL_INDEX)][2] // Check N's South neighbor
                    ) begin
                    group_size <= 2'b10; // Group of 4
                end else if(group_of_2_found[1] && group_of_2_found[3] && //E & W
                         neighbor_values[get_neighbor_index(1, CELL_INDEX)][3] //Check E's West neighbor
                         ) begin
                    group_size <= 2'b10;

                end else if (|group_of_2_found) begin //Any of them
                    group_size <= 2'b01; // Group of 2
                end

            end


            // 3. Prime Implicant Detection (and validity)
            //    This is a simplified version. A full implementation would require
            //    more complex logic to handle all group sizes and overlaps.
            if (cell_value && !prime_implicant_valid) begin // Only re-evaluate if needed
                // Simplified logic:  A cell is a prime implicant if it's not part of a larger group.
                if(group_size == 2'b00 || group_size == 2'b01)
                    is_prime_implicant <= 1'b1;
                else
                    is_prime_implicant <= 1'b0;

                prime_implicant_valid <= 1'b1; // Mark as valid (until a neighbor changes)
            end

            // 4. Invalidate prime_implicant_valid if a neighbor changes
            if (|neighbor_updates) begin
                prime_implicant_valid <= 1'b0;
            end
        end
    end

endmodule


// Parallel Karnaugh Map module
module parallel_kmap #(
    parameter integer NUM_VARIABLES = 4,
    parameter integer KMAP_SIZE = 1 << NUM_VARIABLES // 16 for 4 variables
) (
    input  wire                       clk,
    input  wire                       rst,
    input  wire        [NUM_VARIABLES-1:0]  input_bits, // Input bits (address for K-map)
    input  wire                       input_valid,   // Indicates valid input
    output reg        [NUM_VARIABLES-1:0]  minimized_expression, // Minimized SOP terms
    output reg                        expression_valid,
    output reg                        ready // Ready to accept new input
);

    // Wires to connect cells
    wire [KMAP_SIZE-1:0]                cell_values;
    wire [KMAP_SIZE-1:0][3:0]           neighbor_values;
    wire [KMAP_SIZE-1:0][3:0]           group_signals;
    wire [KMAP_SIZE-1:0][3:0]           neighbor_group_ids;
    wire [KMAP_SIZE-1:0][3:0]           group_ids;

    // Instantiate K-map cells
    genvar i;
    generate
        for (i = 0; i < KMAP_SIZE; i = i + 1) begin : cell_instantiation
            karnaugh_map_cell #(
                .CELL_INDEX(i),
                .NUM_VARIABLES(NUM_VARIABLES)
            ) u_kmap_cell (
                .clk(clk),
                .rst(rst),
                .value_in(cell_values[i]),
                .neighbor_values(neighbor_values[i]),
                .neighbor_group_ids(neighbor_group_ids[i]),
                .cell_value(cell_values[i]),          // Feedback
                .group_signals(group_signals[i]),
                .group_ids(group_ids[i])
            );
        end
    endgenerate

    // --- Neighbor Connections (using Gray code) ---
    generate
        for (i = 0; i < KMAP_SIZE; i = i + 1) begin : neighbor_connections
            // North
            assign neighbor_values[i][0] = cell_values[u_kmap_cell[i].get_neighbor_index(0, i)];
            assign neighbor_group_ids[i][0] = group_ids[u_kmap_cell[i].get_neighbor_index(0, i)][0];
             // East
            assign neighbor_values[i][1] = cell_values[u_kmap_cell[i].get_neighbor_index(1, i)];
            assign neighbor_group_ids[i][1] = group_ids[u_kmap_cell[i].get_neighbor_index(1, i)][1];
             // South
            assign neighbor_values[i][2] = cell_values[u_kmap_cell[i].get_neighbor_index(2, i)];
            assign neighbor_group_ids[i][2] = group_ids[u_kmap_cell[i].get_neighbor_index(2, i)][2];
             // West
            assign neighbor_values[i][3] = cell_values[u_kmap_cell[i].get_neighbor_index(3, i)];
            assign neighbor_group_ids[i][3] = group_ids[u_kmap_cell[i].get_neighbor_index(3, i)][3];

        end
    endgenerate

    // --- K-map Population and Minimization Logic ---
    reg [KMAP_SIZE-1:0] prime_implicants;
    reg [5:0]           state;
    localparam IDLE = 6'b000000;
    localparam POPULATE_KMAP = 6'b000001;
    localparam GROUPING_PASS_1 = 6'b000010; // Groups of 2
    localparam PRIME_IMPLICANT_DETECT  = 6'b000100;
    localparam EXPRESSION_GENERATE = 6'b001000;
    localparam OUTPUT_RESULT  = 6'b010000;

    always @(posedge clk or posedge rst) begin
        if (rst) begin
            state <= IDLE;
            ready <= 1'b1;
            minimized_expression <= {NUM_VARIABLES{1'b0}};
            expression_valid <= 1'b0;
            prime_implicants <= {KMAP_SIZE{1'b0}};
        end else begin
            case (state)
                IDLE: begin
                    expression_valid <= 1'b0;
                    if (input_valid) begin
                        ready <= 1'b0;
                        state <= POPULATE_KMAP;
                    end
                end
                POPULATE_KMAP: begin
                     // Assign input bits to the corresponding K-map cell
                     cell_values[input_bits] <= 1'b1;
                     state <= GROUPING_PASS_1;

                end
                GROUPING_PASS_1: begin // Simplified: Only handles groups of 2
                    // Logic from kmap_cell modules is inherently parallel
                    state <= PRIME_IMPLICANT_DETECT;

                end

                PRIME_IMPLICANT_DETECT: begin
                    //  (Simplified - assumes groups of 2 are prime implicants)
                    for(i = 0; i < KMAP_SIZE; i = i + 1) begin
                       prime_implicants[i] <= u_kmap_cell[i].is_prime_implicant;
                    end

                    state <= EXPRESSION_GENERATE;
                end
                EXPRESSION_GENERATE: begin
                // Generate minimized expression (VERY simplified example)
                minimized_expression <= 4'b0000; // Reset
                //This part needs to be reviewed, it is not correct and does not generate the right expression
                for (i = 0; i < KMAP_SIZE; i = i + 1) begin
                    if (prime_implicants[i]) begin //Simplified example, not the correct function
                        //In a complete system, the correct function must consider prime implicants groups.
                        minimized_expression |= i[3:0];
                    end
                end

                state <= OUTPUT_RESULT;

                end

                OUTPUT_RESULT: begin
                    expression_valid <= 1'b1; // Signal valid output
                    state <= IDLE;
                    ready <= 1'b1;
                end

                default: state <= IDLE;
            endcase
        end
    end
endmodule

// Subcircuit Memory Module
module subcircuit_memory #(
    parameter integer ADDR_WIDTH = 4,
    parameter integer DATA_WIDTH = 4, // Store minimized expression terms
    parameter integer MEMORY_DEPTH = 1 << ADDR_WIDTH
) (
    input  wire                    clk,
    input  wire                    rst,
    input  wire [ADDR_WIDTH-1:0]   address,     // Input bit series as address
    input  wire                    write_enable,
    input  wire [DATA_WIDTH-1:0]   data_in,      // Minimized expression terms
    input  wire                    read_enable,
    output reg  [DATA_WIDTH-1:0]   data_out     // Optimized sub-circuit output
);

    reg [DATA_WIDTH-1:0] memory [MEMORY_DEPTH-1:0];

    // Write operation
    always @(posedge clk) begin
        if (rst) begin
            // Initialize memory (optional, for simulation)
            for (integer i = 0; i < MEMORY_DEPTH; i = i + 1) begin
                memory[i] <= {DATA_WIDTH{1'b0}};
            end
        end else if (write_enable) begin
            memory[address] <= data_in;
        end
    end

    // Read operation (combinational)
     always @(*) begin
        if(read_enable)
            data_out = memory[address];
        else
            data_out = {DATA_WIDTH{1'b0}};
     end
endmodule

// Top-Level Module
module top_module (
    input  wire        clk,
    input  wire        rst,
    input  wire        data_in_valid,
    input  wire [3:0]  data_in,        // Input bit series (4 bits)
    output reg         data_in_ready,
    output reg  [3:0]  data_out,       // Optimized output (4 bits - terms)
    output reg         data_out_valid,
    input wire         data_out_ready
);

    wire [3:0] kmap_minimized_expression;
    wire       kmap_expression_valid;
    wire       kmap_ready;

    // Instantiate the Parallel K-map module
    parallel_kmap u_parallel_kmap (
        .clk(clk),
        .rst(rst),
        .input_bits(data_in),
        .input_valid(data_in_valid && !subcircuit_hit), // Only process if no cache hit
        .minimized_expression(kmap_minimized_expression),
        .expression_valid(kmap_expression_valid),
        .ready(kmap_ready)
    );

    // Instantiate the Subcircuit Memory
    wire [3:0] memory_data_out;
    wire       subcircuit_hit; // Indicates if the subcircuit is in memory
    reg        read_mem;

    subcircuit_memory u_subcircuit_memory (
        .clk(clk),
        .rst(rst),
        .address(data_in),
        .write_enable(kmap_expression_valid), // Write when K-map has a new result
        .data_in(kmap_minimized_expression),
        .read_enable(read_mem),
        .data_out(memory_data_out)
    );

  // State machine for controlling data flow
    reg [2:0] state;
    localparam IDLE = 3'b000;
    localparam KMAP_CALC = 3'b001;
    localparam MEM_READ = 3'b010;
    localparam OUTPUT_RESULT = 3'b011;

    // Check for subcircuit hit (combinational)
    assign subcircuit_hit = (memory_data_out != 0);

     always @(posedge clk or posedge rst) begin
        if(rst) begin
            state <= IDLE;
            data_in_ready <= 1'b1;
            data_out_valid <= 1'b0;
            read_mem <= 1'b0;
            data_out <= 4'b0;
        end else begin
            case (state)
                IDLE: begin
                    data_out_valid <= 1'b0;
                    read_mem <= 1'b0;
                    if(data_in_valid) begin
                        data_in_ready <= 1'b0;
                        read_mem <= 1'b1;
                        state <= MEM_READ;

                    end
                end
                MEM_READ: begin
                    read_mem <= 1'b0; //Done in one cycle
                    if(subcircuit_hit) begin
                        data_out <= memory_data_out;
                        state <= OUTPUT_RESULT;
                    end
                    else begin
                        state <= KMAP_CALC;
                    end
                end
                KMAP_CALC: begin
                    // Wait for K-map calculation to complete
                    if (kmap_expression_valid) begin
                        data_out <= kmap_minimized_expression;
                        state <= OUTPUT_RESULT;
                    end
                end
                OUTPUT_RESULT: begin
                    if(data_out_ready) begin
                        data_out_valid <= 1'b1;
                        state <= IDLE;
                        data_in_ready <= 1'b1;
                    end else begin
                        data_out_valid <= 1'b0;
                    end
                end

            endcase
        end
    end

endmodule

////////////////////////////////
////////////////////////////////
////////////////////////////////

module karnaugh_map_cell_iterative #(
    parameter integer CELL_INDEX = 0,
    parameter integer NUM_VARIABLES = 4
) (
    input  wire        clk,
    input  wire        rst,
    input  wire        write_enable,
    input  wire        value_in,
    input  wire [3:0]  neighbor_values,
    input  wire [3:0]  neighbor_updates,
    input wire [3:0] neighbor_group_sizes_2, // Input groups of 2 from neighbors
    output reg         cell_value,
    output reg  [3:0]  group_signals, // Groups of 2 signals
    output reg         is_prime_implicant,
    output reg         prime_implicant_valid,
    output reg  [1:0]  group_size      // 00=none, 01=2, 10=4, 11=8
);

// ... (gray_encode and get_neighbor_index functions - same as before) ...
    function [3:0] gray_encode;
        input [3:0] binary;
        gray_encode = binary ^ (binary >> 1);
    endfunction

    function [3:0] get_neighbor_index;
        input integer direction; // 0=N, 1=E, 2=S, 3=W
        input [3:0] current_index;
        reg [3:0] neighbor_index;
        reg [3:0] binary_index;
        reg [1:0] row, col;
        reg [1:0] neighbor_row, neighbor_col;

        begin
            binary_index = {current_index[3] ^ current_index[2], current_index[2] ^ current_index[1], current_index[1] ^ current_index[0], current_index[0]};

            row = binary_index[3:2];
            col = binary_index[1:0];

            case (direction)
                0: neighbor_row = (row == 0) ? 2'b11 : row - 1; // North
                1: neighbor_col = (col == 3) ? 2'b00 : col + 1; // East
                2: neighbor_row = (row == 3) ? 2'b00 : row + 1; // South
                3: neighbor_col = (col == 0) ? 2'b11 : col - 1; // West
                default: begin
                            neighbor_row = row;
                            neighbor_col = col;
                         end
            endcase

            if(direction == 0 || direction == 2)
                neighbor_col = col;
            else
                neighbor_row = row;

            neighbor_index = gray_encode({neighbor_row, neighbor_col});
            get_neighbor_index = neighbor_index;

        end
    endfunction

    // Internal state for iterative grouping
    reg [1:0] group_state;
    localparam IDLE       = 2'b00;
    localparam GROUP_2    = 2'b01;
    localparam GROUP_4    = 2'b10;

    reg [3:0] group_of_2; // Store groups of 2

    always @(posedge clk or posedge rst) begin
        if (rst) begin
            cell_value <= 1'b0;
            group_size <= 2'b00;
            is_prime_implicant <= 1'b0;
            prime_implicant_valid <= 1'b0;
            group_signals <= 4'b0000;
            group_state <= IDLE;
            group_of_2 <= 4'b0000;

        end else begin
            if (write_enable) begin
                cell_value <= value_in;
                prime_implicant_valid <= 1'b0;
                group_state <= IDLE; // Restart grouping on write
            end

            if (|neighbor_updates) begin
                prime_implicant_valid <= 1'b0;
                group_state <= IDLE; // Restart on neighbor update
            end


            case (group_state)
                IDLE: begin
                    group_signals <= 4'b0000;
                    group_size <= 2'b00;
                    group_of_2 <= 4'b0000;
                    if(cell_value)
                        group_state <= GROUP_2; // Start grouping if cell is '1'
                end

                GROUP_2: begin // Detect groups of 2
                    group_signals <= 4'b0000; // Clear previous signals
                    group_of_2 <= 4'b0000;
                    if (cell_value) begin
                        if (neighbor_values[0]) begin group_signals[0] <= 1'b1; group_of_2[0] <= 1'b1; end
                        if (neighbor_values[1]) begin group_signals[1] <= 1'b1; group_of_2[1] <= 1'b1; end
                        if (neighbor_values[2]) begin group_signals[2] <= 1'b1; group_of_2[2] <= 1'b1; end
                        if (neighbor_values[3]) begin group_signals[3] <= 1'b1; group_of_2[3] <= 1'b1; end
                    end

                    //Transition to group 4
                    if(|group_of_2)
                        group_size <= 2'b01;
                    else
                        group_size <= 2'b00;

                    group_state <= GROUP_4;

                end

                GROUP_4: begin // Detect groups of 4 (using group_of_2 results)
                    group_size <= 2'b01; //Maintain the value in case there is no group of 4

                    if (cell_value) begin
                        if (group_of_2[0] && group_of_2[2] && // N & S
                            neighbor_values[get_neighbor_index(0, CELL_INDEX)][2] // Check N's South neighbor
                            ) begin
                            group_size <= 2'b10; // Group of 4
                        end else if(group_of_2[1] && group_of_2[3] && //E & W
                                 neighbor_values[get_neighbor_index(1, CELL_INDEX)][3] //Check E's West neighbor
                                 ) begin
                            group_size <= 2'b10;

                        end
                    end

                    group_state <= IDLE;  // Go back to IDLE to be ready for updates

                end

            endcase

            // Simplified prime implicant detection (based on final group_size)
            if (cell_value && !prime_implicant_valid) begin
                is_prime_implicant <= (group_size <= 2'b01); // Prime if not in group of 4 or larger
                prime_implicant_valid <= 1'b1;
            end
        end
    end

endmodule

module kmap_solver #(
    parameter integer NUM_VARIABLES = 4,
    parameter integer KMAP_SIZE = 1 << NUM_VARIABLES
) (
    input  wire                       clk,
    input  wire                       rst,
    input  wire [KMAP_SIZE-1:0]       cell_values,       // Values from all cells
    input  wire [KMAP_SIZE-1:0][3:0]  group_signals,    // Group signals (of 2)
    input  wire [KMAP_SIZE-1:0][1:0]  group_sizes,     // Group sizes (2, 4)
    input  wire [KMAP_SIZE-1:0]       prime_implicant_valids, // Validity flags
    input  wire [KMAP_SIZE-1:0]       is_prime_implicants, // Prime implicant flags
    output reg  [KMAP_SIZE-1:0]       minimized_terms, // Minimized expression (SOP terms)
    output reg                        expression_valid
);

    // Intermediate signals for prime implicant processing
    reg [KMAP_SIZE-1:0] essential_prime_implicants;

    // --- Asynchronous Minimization Logic ---
    always @(*) begin // Combinational logic - continuously updates

        // 1. Essential Prime Implicant Detection (Simplified)
        essential_prime_implicants = {KMAP_SIZE{1'b0}}; // Reset

        // Iterate through all cells
        for (int i = 0; i < KMAP_SIZE; i = i + 1) begin
            //If value is 1 and prime implicant is valid
            if (cell_values[i] && prime_implicant_valids[i] && is_prime_implicants[i]) begin
                // Simplified logic: Mark the cell as part of an essential prime implicant.
                essential_prime_implicants[i] = 1'b1;
            end
        end

        // 2. Generate Minimized Terms (SOP - Simplified)
        minimized_terms = {KMAP_SIZE{1'b0}}; // Reset

        for(int i = 0; i < KMAP_SIZE; i = i + 1) begin
            if (essential_prime_implicants[i]) begin
                minimized_terms[i] = 1'b1;
            end
        end

        // 3. Set expression_valid flag
        // (Simplified: assume valid after one pass. A more robust implementation
        //  would check for stability of the minimized_terms.)
        expression_valid = 1'b1;

    end
endmodule

module kmap_memory_interface #(
    parameter integer NUM_VARIABLES = 4,
    parameter integer KMAP_SIZE = 1 << NUM_VARIABLES
) (
    input  wire                       clk,
    input  wire                       rst,

    // --- External Interface (to io_module or top-level) ---
    input  wire [NUM_VARIABLES-1:0]   address,          // Input bit series (address)
    input  wire                       write_request,    // Write request
    input  wire                       read_request,     // Read request
    input  wire                       write_data,       // Data to write (0 or 1)
    output reg  [KMAP_SIZE-1:0]        read_data,       // Minimized terms
    output reg                        read_data_valid,
    output reg                        busy,              // K-map is busy

    // --- Internal Interface (to K-map cells and solver) ---
    output reg  [KMAP_SIZE-1:0]       cell_write_enables, // To cells
    output reg  [KMAP_SIZE-1:0]       cell_write_data,   // To cells
    input  wire [KMAP_SIZE-1:0]       minimized_terms, // From solver
    input  wire                       expression_valid    // From solver
);

    // Internal state for request handling
    reg [2:0] state;
    localparam IDLE         = 3'b000;
    localparam WRITE_CELL   = 3'b001;
    localparam READ_OUTPUT  = 3'b010;

    always @(posedge clk or posedge rst) begin
        if (rst) begin
            state <= IDLE;
            cell_write_enables <= {KMAP_SIZE{1'b0}};
            cell_write_data <= {KMAP_SIZE{1'b0}};
            read_data <= {KMAP_SIZE{1'b0}};
            read_data_valid <= 1'b0;
            busy <= 1'b0;
        end else begin
            case (state)
                IDLE: begin
                    read_data_valid <= 1'b0;
                    cell_write_enables <= {KMAP_SIZE{1'b0}};
                    if (write_request) begin
                        busy <= 1'b1;
                        // Assert the write enable for the specific cell
                        cell_write_enables[address] <= 1'b1;
                        cell_write_data[address] <= write_data;
                        state <= WRITE_CELL;
                    end else if (read_request) begin
                        busy <= 1'b1;
                        state <= READ_OUTPUT;
                    end
                end

                WRITE_CELL: begin
                    // Deassert write enables after one cycle
                    cell_write_enables <= {KMAP_SIZE{1'b0}};
                    busy <= 1'b0;
                    state <= IDLE;
                end

                READ_OUTPUT: begin
                  // Read from the solver's output (minimized_terms)
                    if (expression_valid) begin
                        read_data <= minimized_terms;
                        read_data_valid <= 1'b1;
                        busy <= 1'b0;
                        state <= IDLE;
                    end
                end
            endcase
        end
    end

endmodule

module top_module_revised (
    input  wire        clk,
    input  wire        rst,
    // --- I/O Interface ---
    input  wire        io_write_request,
    input  wire        io_read_request,
    input  wire [3:0]  io_address,
    input  wire        io_write_data,
    output reg  [15:0] io_read_data,
    output reg         io_read_data_valid,
    output reg         io_busy
);

    // Wires to connect modules
    wire [15:0] kmap_minimized_terms;
    wire        kmap_expression_valid;

    // Instantiate the kmap_memory_interface
    kmap_memory_interface #(
        .NUM_VARIABLES(4)
    ) u_memory_interface (
        .clk(clk),
        .rst(rst),
        .address(io_address),
        .write_request(io_write_request),
        .read_request(io_read_request),
        .write_data(io_write_data),
        .read_data(io_read_data),
        .read_data_valid(io_read_data_valid),
        .busy(io_busy),
        .minimized_terms(kmap_minimized_terms),
        .expression_valid(kmap_expression_valid)
    );

    // --- Instantiate K-map Cells ---
     wire [15:0] cell_values;
     wire [15:0][3:0] neighbor_values;
     wire [15:0][3:0] neighbor_updates; // Connect to cell updates
     wire [15:0][3:0] group_signals_out; // Connect to cell updates
     wire [15:0][1:0] group_sizes_out; // Connect to cell updates
    //Instantiate K-MAP cells
    genvar i;
    generate
        for (i = 0; i < 16; i = i + 1) begin : cell_instantiations
            karnaugh_map_cell_iterative #(
                .CELL_INDEX(i),
                .NUM_VARIABLES(4)
            ) u_kmap_cell (
                .clk(clk),
                .rst(rst),
                .write_enable(u_memory_interface.cell_write_enables[i]),
                .value_in(u_memory_interface.cell_write_data[i]),
                .neighbor_values(neighbor_values[i]),
                .neighbor_updates(neighbor_updates[i]),
                .neighbor_group_sizes_2(), //Not connected yet
                .cell_value(cell_values[i]),
                .group_signals(group_signals_out[i]),
                .is_prime_implicant(),
                .prime_implicant_valid(),
                .group_size(group_sizes_out[i])
            );
        end
    endgenerate

    // --- Connect Neighbors ---
     generate
        for (i = 0; i < 16; i = i + 1) begin : neighbor_connections
            // North
            assign neighbor_values[i][0] = cell_values[u_kmap_cell[i].get_neighbor_index(0, i)];
            assign neighbor_updates[i][0] = u_memory_interface.cell_write_enables[u_kmap_cell[i].get_neighbor_index(0, i)]; // Neighbor update signal
             // East
            assign neighbor_values[i][1] = cell_values[u_kmap_cell[i].get_neighbor_index(1, i)];
            assign neighbor_updates[i][1] = u_memory_interface.cell_write_enables[u_kmap_cell[i].get_neighbor_index(1, i)]; // Neighbor update signal
             // South
            assign neighbor_values[i][2] = cell_values[u_kmap_cell[i].get_neighbor_index(2, i)];
            assign neighbor_updates[i][2] = u_memory_interface.cell_write_enables[u_kmap_cell[i].get_neighbor_index(2, i)]; // Neighbor update signal
             // West
            assign neighbor_values[i][3] = cell_values[u_kmap_cell[i].get_neighbor_index(3, i)];
            assign neighbor_updates[i][3] = u_memory_interface.cell_write_enables[u_kmap_cell[i].get_neighbor_index(3, i)]; // Neighbor update signal

        end
    endgenerate

    // Instantiate the kmap_solver
    kmap_solver #(
        .NUM_VARIABLES(4)
    ) u_kmap_solver (
        .clk(clk),
        .rst(rst),
        .cell_values(cell_values),
        .group_signals(group_signals_out),
        .group_sizes(group_sizes_out),
        .prime_implicant_valids({16{1'b1}}), //For simplification
        .is_prime_implicants({16{1'b0}}),     //For simplification
        .minimized_terms(kmap_minimized_terms),
        .expression_valid(kmap_expression_valid)
    );
endmodule

module io_module_enhanced (
    input  wire        clk,
    input  wire        rst,

    // --- To/From K-map ---
    output reg         kmap_write_request,
    output reg         kmap_read_request,
    output reg  [3:0]  kmap_address,
    output reg         kmap_write_data,
    input  wire [15:0] kmap_read_data,
    input  wire        kmap_read_data_valid,
    input  wire        kmap_busy,

    // --- External Interface (e.g., to a testbench) ---
    input  wire        ext_write,       // External write request
    input  wire        ext_read,        // External read request
    input  wire [3:0]  ext_address,     // External address
    input  wire        ext_write_data,  // External write data
    output reg  [15:0] ext_read_data,    // External read data
    output reg         ext_ready        // Ready for next external request
);
    // Internal state
    reg [1:0] state;
    localparam IDLE = 2'b00;
    localparam KMAP_WRITE = 2'b01;
    localparam KMAP_READ = 2'b10;

     always @(posedge clk or posedge rst) begin
        if (rst) begin
            state <= IDLE;
            kmap_write_request <= 1'b0;
            kmap_read_request <= 1'b0;
            kmap_address <= 4'b0;
            kmap_write_data <= 1'b0;
            ext_read_data <= 16'b0;
            ext_ready <= 1'b1; // Ready initially
        end else begin
            case (state)
                IDLE: begin
                    kmap_write_request <= 1'b0;
                    kmap_read_request <= 1'b0;
                    ext_ready <= 1'b1; // Assume ready unless busy
                    if (ext_write) begin
                        kmap_address <= ext_address;
                        kmap_write_data <= ext_write_data;
                        kmap_write_request <= 1'b1;
                        ext_ready <= 1'b0; // Not ready during K-map operation
                        state <= KMAP_WRITE;
                    end else if (ext_read) begin
                        kmap_address <= ext_address;
                        kmap_read_request <= 1'b1;
                        ext_ready <= 1'b0; // Not ready during K-map operation
                        state <= KMAP_READ;
                    end
                end
                KMAP_WRITE: begin
                    if (!kmap_busy) begin // Wait for K-map to be ready
                        kmap_write_request <= 1'b0; // Deassert request
                        state <= IDLE;
                    end
                end

                KMAP_READ: begin
                    if (kmap_read_data_valid) begin
                        ext_read_data <= kmap_read_data;
                        kmap_read_request <= 1'b0; // Deassert request
                        state <= IDLE;
                    end
                end
            endcase
        end
    end

endmodule

module top_level_system(
    input  wire        clk,
    input  wire        rst,

    // --- External Interface (e.g., to a testbench) ---
    input  wire        ext_write,       // External write request
    input  wire        ext_read,        // External read request
    input  wire [3:0]  ext_address,     // External address
    input  wire        ext_write_data,  // External write data
    output reg  [15:0] ext_read_data,    // External read data
    output reg         ext_ready        // Ready for next external request
    );

    //IO module instantiation
    io_module_enhanced u_io_module (
        .clk(clk),
        .rst(rst),
        .kmap_write_request(),
        .kmap_read_request(),
        .kmap_address(),
        .kmap_write_data(),
        .kmap_read_data(ext_read_data), //Connect the external read data directly
        .kmap_read_data_valid(),
        .kmap_busy(),
        .ext_write(ext_write),
        .ext_read(ext_read),
        .ext_address(ext_address),
        .ext_write_data(ext_write_data),
        .ext_read_data(ext_read_data),
        .ext_ready(ext_ready)
    );

    //Top module instantiation
    top_module_revised u_top_module(
        .clk(clk),
        .rst(rst),
        .io_write_request(ext_write), //Connect directly from external request
        .io_read_request(ext_read),   //Connect directly from external request
        .io_address(ext_address),      //Connect directly from external request
        .io_write_data(ext_write_data), //Connect directly from external request
        .io_read_data(ext_read_data),  //Connect directly from external request
        .io_read_data_valid(ext_ready),//Connect directly from external request
        .io_busy()                     //Connect directly from external request
    );

endmodule