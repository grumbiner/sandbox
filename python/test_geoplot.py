import cartopy.crs as ccrs
import matplotlib.pyplot as plt

ax = plt.axes(projection=ccrs.PlateCarree())
ax.coastlines()

# Save the plot by calling plt.savefig() BEFORE plt.show()
print("trying to savefig",flush=True)

plt.savefig('coastlines.pdf')
print("trying to savefig2",flush=True)
plt.savefig('coastlines.png')

#plt.show()
