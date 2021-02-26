from unfiltered_agg import *
import pandas as pd
import os
import shutil
from lxml import etree
from lxml.etree import XMLParser

if __name__ == '__main__':
    large_parser = XMLParser(huge_tree=True)
    # Initialize flattener class
    iatiflat = IatiFlat()

    # Clean-up of old data; Make sure nothing important is in whatever folder you put here because it will be irrevocably erased
    shutil.rmtree("/home/alex/git/IATI-Covid/ua_sep/")
    os.mkdir("/home/alex/git/IATI-Covid/ua_sep/")

    # Where to look for saved IATI XML via registry-refresher
    rootdir = '/home/alex/git/IATI-Registry-Refresher/data'

    # Predefined header names
    header = iatiflat.header
    full_header = header
    header_frame = pd.DataFrame([full_header])
    header_frame.to_csv("/home/alex/git/IATI-Covid/ua_sep/0000000000000000000header.csv", index=False, header=False, encoding="utf-8")

    # Loop through all the folders downloaded via IATI registry refresh, and pass XML roots to our flatten_activities function.
    for subdir, dirs, files in os.walk(rootdir):
        for filename in files:
            filepath = os.path.join(subdir, filename)
            print(filename)
            try:
                root = etree.parse(filepath, parser=large_parser).getroot()
            except etree.XMLSyntaxError:
                continue
            output = iatiflat.flatten_activities(root)
            if len(output) > 0:
                data = pd.DataFrame(output)
                data.columns = header
                data.to_csv("/home/alex/git/IATI-Covid/ua_sep/{}.csv".format(filename), index=False, header=False, encoding="utf-8")

    # Once individual (headless) CSVs are written for each donor. It's an easy step to concatenate them into one large document.
    # You may want to consider doing this in code rather than saving each donor's CSVs to disk, but I found this useful for
    # saving progress physically in case the conversion process gets interrupted
    os.system("cat /home/alex/git/IATI-Covid/ua_sep/*.csv > /home/alex/git/IATI-Covid/output/iati_unfiltered_agg.csv")
