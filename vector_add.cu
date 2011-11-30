// -*- c -*-

__global__ void add_vectors_kernel(float *A, float *B, float *C, int N) {
    // Determine which element this thread is computing
    int block_id = blockIdx.x + gridDim.x * blockIdx.y;
    int thread_id = blockDim.x * block_id + threadIdx.x;
    
    // Compute a single element of the result vector (if the element is valid)
    if (thread_id < N) C[thread_id] = A[thread_id] + B[thread_id];
}

// Returns the vector sum A + B (computed on the GPU)
float *GPU_add_vectors(float *A_CPU, float *B_CPU, int N) {
    
    // Allocate GPU memory for the inputs and the result
    int vector_size = N * sizeof(float);
    float *A_GPU, *B_GPU, *C_GPU;
    cudaMalloc((void **) &A_GPU, vector_size);
    cudaMalloc((void **) &B_GPU, vector_size);
    cudaMalloc((void **) &C_GPU, vector_size);
    
    // Transfer the input vectors to GPU memory
    cudaMemcpy(A_GPU, A_CPU, vector_size, cudaMemcpyHostToDevice);
    cudaMemcpy(B_GPU, B_CPU, vector_size, cudaMemcpyHostToDevice);
    
    // Determine the number of thread blocks in the x- and y-dimension
    int num_blocks = (int) ((float) (N + threads_per_block - 1) / (float) threads_per_block);
    int max_blks_per_dim = 65535;
    int num_blocks_y = (int) ((float) (num_blocks + max_blocks_per_dimension - 1) / (float) max_blks_per_dim);
    int num_blocks_x = (int) ((float) (num_blocks + num_blocks_y - 1) / (float) num_blocks_y);
    dim3 grid_size(num_blocks_x, num_blocks_y, 1);
    
    // Execute the kernel to compute the vector sum on the GPU
    add_vectors_kernel <<< grid_size , threads_per_block >>> (A_GPU, B_GPU, C_GPU, N);
    
    // Allocate CPU memory for the result
    float *C_CPU = (float *) malloc(vector_size);
    
    // Transfer the result from the GPU to the CPU
    cudaMemcpy(C_CPU, C_GPU, vector_size, cudaMemcpyDeviceToHost);
    
    // Free the GPU memory
    cudaFree(A_GPU);
    cudaFree(B_GPU);
    cudaFree(C_GPU);
    
    return C_CPU;
}

