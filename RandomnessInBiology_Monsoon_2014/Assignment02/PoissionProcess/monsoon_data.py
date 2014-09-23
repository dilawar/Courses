"""monsoon_data.py: Get monsoon data.

Last modified: Sat Jan 18, 2014  05:01PM

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2013, Dilawar Singh and NCBS Bangalore"
__credits__          = ["NCBS Bangalore"]
__license__          = "GNU GPL"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import mechanize

baseUrl = "http://www.imd.gov.in/section/hydro/distrainfall/up.html"
br = mechanize.Browser()
br.open(baseUrl)

def main():
    print("Downloading data")
    links = br.links()
    for l in links:
        download_link = "/".join(l.base_url.split('/')[:-1])+'/'+l.url
        print("[INFO] Downloading link {}".format(download_link))
        dir = os.path.dirname(l.url)
        if not os.path.isdir(dir):
            os.makedirs(dir)
        try:
            br.retrieve(download_link, l.url)
        except Exception as e:
            print("[INFO] Failed to download {}".format(download_link))
            print("++ Error was {}".format(e))

if __name__ == '__main__':
    import sys
    import os
    main()
