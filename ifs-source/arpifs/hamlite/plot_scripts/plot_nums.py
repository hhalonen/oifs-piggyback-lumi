import matplotlib.pyplot as plt
import numpy as np
import os

emis = 'F' #T for emissions on (F for off)
ddep = 'F' #T for drydep on (F for off)
wdep = 'T' #T for wetdep on (F for off)
sedi = 'F' #T for sedimentation on (F for off)
radi = 'T' #T for radiation on (F for off)


# Path to your .dat file
file_path = f'../data/num_hamlite_emis{emis}_ddep{ddep}_wdep{wdep}_sedi{sedi}_radi{radi}.dat'

# Read file
with open(file_path, 'r') as f:
    lines = f.readlines()

# First line contains headers
headers = lines[0].strip().split()

# Remaining lines contain data
data = []
for line in lines[1:]:
    if line.strip():  # Skip empty lines
        row = [float(x) for x in line.strip().split()]
        data.append(row)

# Convert to NumPy array
data = np.array(data)
num_rows, num_cols = data.shape
print(data)
# Create subplots
fig, axs = plt.subplots(num_cols, 1, figsize=(8, 2 * num_cols), sharex=True)

#ylims = [1.5e5, 5.2e4, 1.1e6, 1.7e6]
#ylows = [-1e4, -5e3, -1.1e4, -1.7e4]
ylims = [4.3e+11, 0.000e+00, 8.2e+07, 1.9e+09]
ylows = [0.0, 0.0, 0.0, 0.0]

# Plot each column in its own subplot
for i,yh,yl in zip(range(num_cols),ylims,ylows):
    axs[i].plot(range(num_rows), data[:, i], label=headers[i])
    axs[i].set_ylabel(headers[i])
    axs[i].legend(loc='upper right')
    axs[i].grid(True)
    # Plot settings
    if i==num_cols-1:
        axs[i].set_xlabel("Time")
    axs[i].set_ylabel("#/kg")
    #axs[i].set_ylim(yl,yh)
    #axs[i].set_title("HAM-lite data profiles")
    axs[i].legend()
    axs[i].grid(True)
    #axs[i].tight_layout()

axs[-1].set_xlabel('Time')

plt.tight_layout()

newpath = "timeseries_compounds"
print(newpath)
if not os.path.exists(newpath): #checks if the newpath exists and makes the directory if it doesnt exist
    os.makedirs(newpath)
plt.savefig(f'{newpath}/hamlite_timeseries_emis{emis}_ddep{ddep}_wdep{wdep}_sedi{sedi}_radi{radi}.png', bbox_inches = 'tight', pad_inches = 0.1) #saves the figure in the newpath directory
plt.close()

plt.show()