# Import the library
import psutil

# Display CPU usage using psutil.cpu_percent
print('')
print('CPU Usage')
try:
    cpu_percent = psutil.cpu_percent()
    print(f"Percentage usage: {cpu_percent}%")
    
    # CPU frequency
    cpu_info = psutil.cpu_freq()
    print(f"Current frequency: {cpu_info.current:.2f} Mhz")
    print(f"Minimum frequency: {cpu_info.min:.2f} Mhz")
    print(f"Maximum frequency: {cpu_info.max:.2f} Mhz")
    print('')
except FileNotFoundError:
    print("CPU info not available on this system")

# Display RAM sage using psutil.virtual_memory
print('RAM Usage')
try:
    ram_info = psutil.virtual_memory()
    print(f"Total: {ram_info.total / 1024 / 1024 / 1024:.2f} GB")
    print(f"Available: {ram_info.available / 1024 / 1024 / 1024:.2f} GB")
    print(f"Used: {ram_info.used / 1024 / 1024 / 1024:.2f} GB")
    print(f"Percentage usage: {ram_info.percent}%")
    print('')
except FileNotFoundError:
    print("Ram info not available on this system")

# Display disk usage using psutil.disk_usage
print('Disk Usage')
try:
    disk_info = psutil.disk_usage("/")
    print(f"Total: {disk_info.total / 1024 / 1024 / 1024:.2f} GB")
    print(f"Used: {disk_info.used / 1024 / 1024 / 1024:.2f} GB")
    print(f"Free: {disk_info.free / 1024 / 1024 / 1024:.2f} GB")
    print('')
except FileNotFoundError:
    print("Disk info not available on this system")
