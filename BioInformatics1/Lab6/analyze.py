import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
try:
    mpl.style.use( 'seaborn-talk' )
except Exception as e:
    print(e)


df = pd.read_csv('./datamonkey-table.csv')
df['dN-dS'] = df['dN'] - df['dS']
ax1 = plt.subplot(2, 1, 1)
df.plot(x='Site', y='dN-dS', kind='bar', ax=ax1)

dnds = df['dN-dS']
print(dnds.max(), dnds.min())
print(dnds.argmax(), dnds.argmin())

ax2 = plt.subplot(2, 1, 2)

outfile = 'lab6.pdf'
plt.savefig(outfile)
print(f'[INFO] Saved to {outfile}')
