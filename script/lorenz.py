import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

# Lorenz system parameters
sigma = 10.0
rho = 28.0
beta = 8.0 / 3.0

# Lorenz system equations
def lorentz_system(x, y, z, sigma, rho, beta):
    dx = sigma * (y - x)
    dy = x * (rho - z) - y
    dz = x * y - beta * z
    return dx, dy, dz

# Time step and number of iterations
dt = 0.01
N = 12000

# Initialize variables
x = np.empty(N)
y = np.empty(N)
z = np.empty(N)

# Initial conditions
x[0], y[0], z[0] = (0, 0.1, 0)

# Time evolution
for i in range(N - 1):
    dx, dy, dz = lorentz_system(x[i], y[i], z[i], sigma, rho, beta)
    x[i + 1] = x[i] + (dx * dt)
    y[i + 1] = y[i] + (dy * dt)
    z[i + 1] = z[i] + (dz * dt)
    
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
ax.plot(x, y, z, lw=0.45,color='#aec4ca')
ax.set_axis_off()
fig.patch.set_alpha(0)
plt.savefig('../figure/lorenz1.png',dpi = 300,transparent=True)

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
ax.plot(x, y, z, lw=0.45,color='#fabcbd')
ax.set_axis_off()
fig.patch.set_alpha(0)
plt.savefig('../figure/lorenz2.png',dpi = 300,transparent=True)

m1 = pd.read_csv('../result/phase_space1.csv')
m2 = pd.read_csv('../result/phase_space2.csv')

m1 = pd.read_csv('../result/phase_space1.csv')
m2 = pd.read_csv('../result/phase_space2.csv')

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
ax.scatter(m1["x"], m1["y"], m1["z"], color='#aec4ca')
ax.set_xticklabels([])
ax.set_yticklabels([])
ax.set_zticklabels([])
fig.patch.set_alpha(0)
plt.savefig('../figure/state1.jpg',dpi = 300,transparent=True)

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
ax.scatter(m2["x"], m2["y"], m2["z"], color='#fabcbd')
ax.set_xticklabels([])
ax.set_yticklabels([])
ax.set_zticklabels([])
fig.patch.set_alpha(0)
plt.savefig('../figure/state2.jpg',dpi = 300,transparent=True)

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
ax.scatter(m1["x"], m1["y"], m1["z"], color='#aec4ca')
ax.set_axis_off()
fig.patch.set_alpha(0)
plt.savefig('../figure/phase_state1.png',dpi = 300,transparent=True)

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
ax.scatter(m2["x"], m2["y"], m2["z"], color='#fabcbd')
ax.set_axis_off()
fig.patch.set_alpha(0)
plt.savefig('../figure/phase_state2.png',dpi = 300,transparent=True)

embeddings = pd.read_csv('../result/figure1_embeddings.csv')

fig = plt.figure(figsize=(4.5, 4), dpi=300)
ax = fig.add_subplot(111, projection='3d')
ax.scatter(embeddings["hs1"], embeddings["hs2"], embeddings["hs3"], s=2.75, color='#fc4e2a')
ax.set_xlabel(r"$h_{s(1)}(x)$",fontsize=12)
ax.set_ylabel(r"$h_{s(2)}(x)$",fontsize=12)
ax.set_zlabel(r"$h_{s(3)}(x)$",fontsize=12)
fig.patch.set_alpha(0)
plt.savefig('../figure/figure1_3.jpg',dpi = 300,transparent=True)
