attach(Intel_CPUs)

# This shows the dimensions: num_row x num_col
dim(Intel_CPUs)

# This show some basic info
glimpse(Intel_CPUs)

# This list the names of columns / features
names(Intel_CPUs)

unique(Product_Collection) # 0 kha thi

unique(Vertical_Segment) 
# Mobile, Desktop, Embedded, Server
# -> may change to factor
# - Factor is used to work with categorical variables
# variables that have fixed and known set of possible values

unique(Processor_Number) # 0 kha thi

unique(Status) 
# Launched, End of Life, End of Interactive Support, Announced
# -> may change to factor

unique(Launch_Date) # ok, but has NA, 1 wrong format: "Q1 '15"

unique(Lithography) # ok, but has NA, others in right format, unit: nanometer

unique(Recommended_Customer_Price)
# has "N/A"
# some in wrong format such as "$123.00 - $123.00"
# -> may treat this as take the mean of that range ??
# right format should be: "$123.00"

unique(nb_of_Cores)
# ok, 22 values in the domain
# 2  4  1  6  8 10 12 14 16 18 72 57 20 22 60 61 68 64 24 26 28 15

unique(nb_of_Threads)
# ok, same as above
# but has NA, other 19 values in the domain

unique(Processor_Base_Frequency)
# hard to calculate but possible
# - Has NA
# - Some in unit of GHz, MHz
# -> may choose MHz to be the common one to calculate

unique(Max_Turbo_Frequency)
# All in GHz, Has NA

unique(Cache) # ko kha thi, do co 111 value khac nhau, do vi cung khac nhau

unique(Bus_Speed) 
# ko kha thi mac du chi co 35 values la vi co NA va
# unit tui no cung khac nhau nhieu khong dong nhat duoc

unique(TDP)
# ko NA, all in right format of "<val> W"

unique(Embedded_Options_Available) # 0 kha thi: No, Yes, NA

unique(Conflict_Free) # 0 kha thi: Yes, NA

unique(Max_Memory_Size)
# - Has NA
# - Others has different unit, GB / TB, might be ok if NA not much

unique(Memory_Types) # 0 kha thi

unique(Max_nb_of_Memory_Channels)
# might be ok
# 2 4 1 NA 6 12 3 16

unique(Max_Memory_Bandwidth)
# might be ok
# - Has NA, other in format "<value> GB/s"

unique(ECC_Memory_Supported) # 0 kha thi: yes, no, NA

unique(Processor_Graphics_) # 0 kha thi: NA

unique(Graphics_Base_Frequency)
# - might be ok
# - Has NA, others in unit: MHz / GHz

unique(Graphics_Max_Dynamic_Frequency)
# - Like above, might be ok
# - has NA, others in unit: MHz / GHz

unique(Graphics_Video_Max_Memory)
# - Might be ok
# - Has NA, others in unit GB / MB

unique(Graphics_Output) # 0 kha thi, value ki quac : >

unique(Support_4k) # 0 kha thi: NA

unique(Max_Resolution_HDMI) # 0 kha thi
unique(Max_Resolution_DP) # 0 kha thi
unique(Max_Resolution_eDP_Integrated_Flat_Panel) # 0 kha thi
unique(DirectX_Support) # 0 kha thi
unique(OpenGL_Support) # 0 kha thi
unique(PCI_Express_Revision) # 0 kha thi
unique(PCI_Express_Configurations_) # cang kh kha thi :))

unique(Max_nb_of_PCI_Express_Lanes) 
# - maybe ok
# 10 12 40  4 NA  1  6 16 24 20 28 44  2  8 32  0 48 36

unique(T)
# - quan trong nhung ma data format hoi ki
# - Chu yeu unit la oC
# - Has NA
# - mot vai thg co format: "10C" (y la bi thieu o)
# - "C1=69.2°C; D0=64.4°C"
# - "90C (PGA); 105C (BGA)"
# - "100C (BGA)"
# - "90°C for rPGA,105°C for BGA" 
# - "B3=62.2°C; G0=71°C"
# - "L2=61.4°C, M0=73.3°C"
# - "C1+D1=74°C; M0=72°C"
# - "79.5°C - 94.5°C"
# - mot vai thg 0 co unit

# Khuc nay do minh lam dataset Intel nen hong ro nay co can kh
unique(Intel_Hyper_Threading_Technology_) # 0 kha thi: yes no NA
unique(Intel_Virtualization_Technology_VTx_) # 0 kha thi: nhu tren
unique(Intel_64_) # yes no NA
unique(Instruction_Set) 
# not sure
# "64-bit" "32-bit" NA "Itanium 64-bit"

unique(Instruction_Set_Extensions) # 0 kha thi
unique(Idle_States) # 0 kha thi: yes no na
unique(Thermal_Monitoring_Technologies) # 0 kha thi: yes no na
unique(Secure_Key) # 0 kha thi: yes no na
unique(Execute_Disable_Bit) # 0 kha thi: yes no na 



names(Intel_CPUs)



