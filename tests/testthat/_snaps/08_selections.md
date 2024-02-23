# calc_selection_weights()

    {
      "type": "list",
      "attributes": {
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["file_name", "type", "path", "aru_type", "aru_id", "site_id", "date_time", "date", "longitude", "latitude", "tz", "t2sr", "t2ss", "doy", "psel_min", "psel_doy", "psel", "psel_scaled", "psel_std", "psel_normalized"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["P01_1_20200503T052000_ARU.wav", "P01_1_20200503T052000_ARU.wav", "P01_1_20200503T052000_ARU.wav", "P02_1_20200504T052500_ARU.wav", "P02_1_20200505T073000_ARU.wav", "P02_1_20200504T052500_ARU.wav", "P02_1_20200505T073000_ARU.wav", "P02_1_20200504T052500_ARU.wav", "P02_1_20200505T073000_ARU.wav", "P03_1_20200506T100000_ARU.wav", "P03_1_20200506T100000_ARU.wav", "P03_1_20200506T100000_ARU.wav", "P06_1_20200509T052000_ARU.wav", "P06_1_20200509T052000_ARU.wav", "P06_1_20200509T052000_ARU.wav", "P07_1_20200509T052500_ARU.wav", "P07_1_20200510T073000_ARU.wav", "P07_1_20200509T052500_ARU.wav", "P07_1_20200510T073000_ARU.wav", "P07_1_20200509T052500_ARU.wav", "P07_1_20200510T073000_ARU.wav", "P08_1_20200511T100000_ARU.wav", "P08_1_20200511T100000_ARU.wav", "P08_1_20200511T100000_ARU.wav", "P09_1_20200511T050000_ARU.wav", "P09_1_20200511T050000_ARU.wav", "P09_1_20200511T050000_ARU.wav"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["a_BARLT10962_P01_1/P01_1_20200503T052000_ARU.wav", "j_BARLT10962_P01_1/P01_1_20200503T052000_ARU.wav", "o_BARLT10962_P01_1/P01_1_20200503T052000_ARU.wav", "a_S4A01234_P02_1/P02_1_20200504T052500_ARU.wav", "a_S4A01234_P02_1/P02_1_20200505T073000_ARU.wav", "j_S4A01234_P02_1/P02_1_20200504T052500_ARU.wav", "j_S4A01234_P02_1/P02_1_20200505T073000_ARU.wav", "o_S4A01234_P02_1/P02_1_20200504T052500_ARU.wav", "o_S4A01234_P02_1/P02_1_20200505T073000_ARU.wav", "a_BARLT10962_P03_1/P03_1_20200506T100000_ARU.wav", "j_BARLT10962_P03_1/P03_1_20200506T100000_ARU.wav", "o_BARLT10962_P03_1/P03_1_20200506T100000_ARU.wav", "a_BARLT10962_P06_1/P06_1_20200509T052000_ARU.wav", "j_BARLT10962_P06_1/P06_1_20200509T052000_ARU.wav", "o_BARLT10962_P06_1/P06_1_20200509T052000_ARU.wav", "a_S4A01234_P07_1/P07_1_20200509T052500_ARU.wav", "a_S4A01234_P07_1/P07_1_20200510T073000_ARU.wav", "j_S4A01234_P07_1/P07_1_20200509T052500_ARU.wav", "j_S4A01234_P07_1/P07_1_20200510T073000_ARU.wav", "o_S4A01234_P07_1/P07_1_20200509T052500_ARU.wav", "o_S4A01234_P07_1/P07_1_20200510T073000_ARU.wav", "a_BARLT10962_P08_1/P08_1_20200511T100000_ARU.wav", "j_BARLT10962_P08_1/P08_1_20200511T100000_ARU.wav", "o_BARLT10962_P08_1/P08_1_20200511T100000_ARU.wav", "a_S4A02222_P09_1/P09_1_20200511T050000_ARU.wav", "j_S4A02222_P09_1/P09_1_20200511T050000_ARU.wav", "o_S4A02222_P09_1/P09_1_20200511T050000_ARU.wav"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["BarLT", "BarLT", "BarLT", "SongMeter", "SongMeter", "SongMeter", "SongMeter", "SongMeter", "SongMeter", "BarLT", "BarLT", "BarLT", "BarLT", "BarLT", "BarLT", "SongMeter", "SongMeter", "SongMeter", "SongMeter", "SongMeter", "SongMeter", "BarLT", "BarLT", "BarLT", "SongMeter", "SongMeter", "SongMeter"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["BARLT10962", "BARLT10962", "BARLT10962", "S4A01234", "S4A01234", "S4A01234", "S4A01234", "S4A01234", "S4A01234", "BARLT10962", "BARLT10962", "BARLT10962", "BARLT10962", "BARLT10962", "BARLT10962", "S4A01234", "S4A01234", "S4A01234", "S4A01234", "S4A01234", "S4A01234", "BARLT10962", "BARLT10962", "BARLT10962", "S4A02222", "S4A02222", "S4A02222"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["P01_1", "P01_1", "P01_1", "P02_1", "P02_1", "P02_1", "P02_1", "P02_1", "P02_1", "P03_1", "P03_1", "P03_1", "P06_1", "P06_1", "P06_1", "P07_1", "P07_1", "P07_1", "P07_1", "P07_1", "P07_1", "P08_1", "P08_1", "P08_1", "P09_1", "P09_1", "P09_1"]
        },
        {
          "type": "double",
          "attributes": {
            "tzone": {
              "type": "character",
              "attributes": {},
              "value": ["UTC"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["POSIXct", "POSIXt"]
            }
          },
          "value": [1588483200, 1588483200, 1588483200, 1588569900, 1588663800, 1588569900, 1588663800, 1588569900, 1588663800, 1588759200, 1588759200, 1588759200, 1589001600, 1589001600, 1589001600, 1589001900, 1589095800, 1589001900, 1589095800, 1589001900, 1589095800, 1589191200, 1589191200, 1589191200, 1589173200, 1589173200, 1589173200]
        },
        {
          "type": "double",
          "attributes": {
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["Date"]
            }
          },
          "value": [18385, 18385, 18385, 18386, 18387, 18386, 18387, 18386, 18387, 18388, 18388, 18388, 18391, 18391, 18391, 18391, 18392, 18391, 18392, 18391, 18392, 18393, 18393, 18393, 18393, 18393, 18393]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-85.03, -85.03, -85.03, -87.45, -87.45, -87.45, -87.45, -87.45, -87.45, -90.38, -90.38, -90.38, -90.08, -90.08, -90.08, -86.03, -86.03, -86.03, -86.03, -86.03, -86.03, -84.45, -84.45, -84.45, -91.38, -91.38, -91.38]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [50.01, 50.01, 50.01, 52.68, 52.68, 52.68, 52.68, 52.68, 52.68, 48.99, 48.99, 48.99, 52, 52, 52, 50.45, 50.45, 50.45, 50.45, 50.45, 50.45, 48.999, 48.999, 48.999, 45, 45, 45]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Winnipeg", "America/Winnipeg", "America/Winnipeg", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Chicago", "America/Chicago", "America/Chicago"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-53.21666667, -53.21666667, -53.21666667, -47.25, 79.61666667, -47.25, 79.61666667, -47.25, 79.61666667, 207.13333333, 207.13333333, 207.13333333, 3.58333333, 3.58333333, 3.58333333, -40.93333333, 85.61666667, -40.93333333, 85.61666667, -40.93333333, 85.61666667, 238.31666667, 238.31666667, 238.31666667, -41.88333333, -41.88333333, -41.88333333]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [498.41666667, 498.41666667, 498.41666667, 483.41666667, 606.68333333, 483.41666667, 606.68333333, 483.41666667, 606.68333333, -685.88333333, -685.88333333, -685.88333333, 521.93333333, 521.93333333, 521.93333333, 488.75, 612.23333333, 488.75, 612.23333333, 488.75, 612.23333333, -669.31666667, -669.31666667, -669.31666667, 516.65, 516.65, 516.65]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [124, 124, 124, 125, 126, 125, 126, 125, 126, 127, 127, 127, 130, 130, 130, 130, 131, 130, 131, 130, 131, 132, 132, 132, 132, 132, 132]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-0.53599721, -0.53599721, -0.53599721, -0.52402648, -0.48126855, -0.52402648, -0.48126855, -0.52402648, -0.48126855, -0.84075194, -0.84075194, -0.84075194, -0.45852417, -0.45852417, -0.45852417, -0.51295356, -0.48919915, -0.51295356, -0.48919915, -0.51295356, -0.48919915, -0.98957549, -0.98957549, -0.98957549, -0.5147367, -0.5147367, -0.5147367]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-0.93517202, -0.93517202, -0.93517202, -0.92000393, -0.90525141, -0.92000393, -0.90525141, -0.92000393, -0.90525141, -0.89091445, -0.89091445, -0.89091445, -0.85039697, -0.85039697, -0.85039697, -0.85039697, -0.83772227, -0.85039697, -0.83772227, -0.85039697, -0.83772227, -0.82546313, -0.82546313, -0.82546313, -0.82546313, -0.82546313, -0.82546313]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.22965681, 0.22965681, 0.22965681, 0.23597476, 0.24994361, 0.23597476, 0.24994361, 0.23597476, 0.24994361, 0.17698923, 0.17698923, 0.17698923, 0.27011131, 0.27011131, 0.27011131, 0.25580227, 0.26529273, 0.25580227, 0.26529273, 0.25580227, 0.26529273, 0.16283162, 0.16283162, 0.16283162, 0.26179335, 0.26179335, 0.26179335]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.85023024, 0.85023024, 0.85023024, 0.87362044, 0.92533557, 0.87362044, 0.92533557, 0.87362044, 0.92533557, 0.65524553, 0.65524553, 0.65524553, 1, 1, 1, 0.94702537, 0.98216075, 0.94702537, 0.98216075, 0.94702537, 0.98216075, 0.60283154, 0.60283154, 0.60283154, 0.96920542, 0.96920542, 0.96920542]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 0.94411202, 1, 0.94411202, 1, 0.94411202, 1, 1, 1, 1, 1, 1, 1, 0.96422644, 1, 0.96422644, 1, 0.96422644, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "double",
          "attributes": {},
          "value": ["NaN", "NaN", "NaN", 0.001, 1, 0.001, 1, 0.001, 1, "NaN", "NaN", "NaN", "NaN", "NaN", "NaN", 0.001, 1, 0.001, 1, 0.001, 1, "NaN", "NaN", "NaN", "NaN", "NaN", "NaN"]
        }
      ]
    }

# sample_recordings()

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["sites_legacy", "sites_base", "sites_over", "sites_near", "design"]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["sp_design"]
        }
      },
      "value": [
        {
          "type": "NULL"
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["siteID", "siteuse", "replsite", "lon_WGS84", "lat_WGS84", "stratum", "wgt", "ip", "caty", "aux", "file_name", "type", "path", "aru_type", "aru_id", "site_id", "date_time", "date", "tz", "t2sr", "t2ss", "doy", "psel_min", "psel_doy", "psel", "psel_scaled", "psel_std", "psel_normalized", "geometry"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["sf", "data.frame"]
            },
            "sf_column": {
              "type": "character",
              "attributes": {},
              "value": ["geometry"]
            },
            "agr": {
              "type": "integer",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["siteID", "siteuse", "replsite", "lon_WGS84", "lat_WGS84", "stratum", "wgt", "ip", "caty", "aux", "file_name", "type", "path", "aru_type", "aru_id", "site_id", "date_time", "date", "tz", "t2sr", "t2ss", "doy", "psel_min", "psel_doy", "psel", "psel_scaled", "psel_std", "psel_normalized"]
                },
                "levels": {
                  "type": "character",
                  "attributes": {},
                  "value": ["constant", "aggregate", "identity"]
                },
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["factor"]
                }
              },
              "value": ["NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["sample-01", "sample-02", "sample-03", "sample-04", "sample-05", "sample-06", "sample-07", "sample-08", "sample-09", "sample-10", "sample-11", "sample-12"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Base", "Base", "Base", "Base", "Base", "Base", "Base", "Base", "Base", "Base", "Base", "Base"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["None", "None", "None", "None", "None", "None", "None", "None", "None", "None", "None", "None"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-84.45, -86.03, -87.45, -90.38, -86.03, -90.08, -84.45, -86.03, -87.45, -91.38, -85.03, -87.45]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [48.999, 50.45, 52.68, 48.99, 50.45, 52, 48.999, 50.45, 52.68, 45, 50.01, 52.68]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["None", "None", "None", "None", "None", "None", "None", "None", "None", "None", "None", "None"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [2.22708462, 2.3097112, 2.35891989, 2.22708462, 2.3097112, 2.22708462, 2.22708462, 2.22708462, 2.35891989, 2.22708462, 2.22708462, 2.35891989]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.44901752, 0.43295456, 0.42392283, 0.44901752, 0.43295456, 0.44901752, 0.44901752, 0.44901752, 0.42392283, 0.44901752, 0.44901752, 0.42392283]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["None", "None", "None", "None", "None", "None", "None", "None", "None", "None", "None", "None"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1, 0.96422644, 0.94411202, 1, 0.96422644, 1, 1, 1, 0.94411202, 1, 1, 0.94411202]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["P08_1_20200511T100000_ARU.wav", "P07_1_20200509T052500_ARU.wav", "P02_1_20200504T052500_ARU.wav", "P03_1_20200506T100000_ARU.wav", "P07_1_20200509T052500_ARU.wav", "P06_1_20200509T052000_ARU.wav", "P08_1_20200511T100000_ARU.wav", "P07_1_20200510T073000_ARU.wav", "P02_1_20200504T052500_ARU.wav", "P09_1_20200511T050000_ARU.wav", "P01_1_20200503T052000_ARU.wav", "P02_1_20200504T052500_ARU.wav"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["a_BARLT10962_P08_1/P08_1_20200511T100000_ARU.wav", "a_S4A01234_P07_1/P07_1_20200509T052500_ARU.wav", "j_S4A01234_P02_1/P02_1_20200504T052500_ARU.wav", "o_BARLT10962_P03_1/P03_1_20200506T100000_ARU.wav", "j_S4A01234_P07_1/P07_1_20200509T052500_ARU.wav", "o_BARLT10962_P06_1/P06_1_20200509T052000_ARU.wav", "o_BARLT10962_P08_1/P08_1_20200511T100000_ARU.wav", "o_S4A01234_P07_1/P07_1_20200510T073000_ARU.wav", "o_S4A01234_P02_1/P02_1_20200504T052500_ARU.wav", "a_S4A02222_P09_1/P09_1_20200511T050000_ARU.wav", "o_BARLT10962_P01_1/P01_1_20200503T052000_ARU.wav", "a_S4A01234_P02_1/P02_1_20200504T052500_ARU.wav"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["BarLT", "SongMeter", "SongMeter", "BarLT", "SongMeter", "BarLT", "BarLT", "SongMeter", "SongMeter", "SongMeter", "BarLT", "SongMeter"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["BARLT10962", "S4A01234", "S4A01234", "BARLT10962", "S4A01234", "BARLT10962", "BARLT10962", "S4A01234", "S4A01234", "S4A02222", "BARLT10962", "S4A01234"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["P08_1", "P07_1", "P02_1", "P03_1", "P07_1", "P06_1", "P08_1", "P07_1", "P02_1", "P09_1", "P01_1", "P02_1"]
            },
            {
              "type": "double",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["POSIXct", "POSIXt"]
                },
                "tzone": {
                  "type": "character",
                  "attributes": {},
                  "value": ["UTC"]
                }
              },
              "value": [1589191200, 1589001900, 1588569900, 1588759200, 1589001900, 1589001600, 1589191200, 1589095800, 1588569900, 1589173200, 1588483200, 1588569900]
            },
            {
              "type": "double",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Date"]
                }
              },
              "value": [18393, 18391, 18386, 18388, 18391, 18391, 18393, 18392, 18386, 18393, 18385, 18386]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Winnipeg", "America/Toronto", "America/Toronto", "America/Toronto", "America/Chicago", "America/Toronto", "America/Toronto"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [238.31666667, -40.93333333, -47.25, 207.13333333, -40.93333333, 3.58333333, 238.31666667, 85.61666667, -47.25, -41.88333333, -53.21666667, -47.25]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-669.31666667, 488.75, 483.41666667, -685.88333333, 488.75, 521.93333333, -669.31666667, 612.23333333, 483.41666667, 516.65, 498.41666667, 483.41666667]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [132, 130, 125, 127, 130, 130, 132, 131, 125, 132, 124, 125]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.98957549, -0.51295356, -0.52402648, -0.84075194, -0.51295356, -0.45852417, -0.98957549, -0.48919915, -0.52402648, -0.5147367, -0.53599721, -0.52402648]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.82546313, -0.85039697, -0.92000393, -0.89091445, -0.85039697, -0.85039697, -0.82546313, -0.83772227, -0.92000393, -0.82546313, -0.93517202, -0.92000393]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.16283162, 0.25580227, 0.23597476, 0.17698923, 0.25580227, 0.27011131, 0.16283162, 0.26529273, 0.23597476, 0.26179335, 0.22965681, 0.23597476]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.60283154, 0.94702537, 0.87362044, 0.65524553, 0.94702537, 1, 0.60283154, 0.98216075, 0.87362044, 0.96920542, 0.85023024, 0.87362044]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1, 0.96422644, 0.94411202, 1, 0.96422644, 1, 1, 1, 0.94411202, 1, 1, 0.94411202]
            },
            {
              "type": "double",
              "attributes": {},
              "value": ["NaN", 0.001, 0.001, "NaN", 0.001, "NaN", "NaN", 1, 0.001, "NaN", "NaN", 0.001]
            },
            {
              "type": "list",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["sfc_POINT", "sfc"]
                },
                "precision": {
                  "type": "double",
                  "attributes": {},
                  "value": [0]
                },
                "bbox": {
                  "type": "double",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["xmin", "ymin", "xmax", "ymax"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["bbox"]
                    }
                  },
                  "value": [427866.72388268, 12063887.17557167, 970121.15372838, 12897505.49960899]
                },
                "crs": {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["input", "wkt"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["crs"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["EPSG:3161"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["PROJCRS[\"NAD83 / Ontario MNR Lambert\",\n    BASEGEOGCRS[\"NAD83\",\n        DATUM[\"North American Datum 1983\",\n            ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4269]],\n    CONVERSION[\"Ontario MNR Lambert\",\n        METHOD[\"Lambert Conic Conformal (2SP)\",\n            ID[\"EPSG\",9802]],\n        PARAMETER[\"Latitude of false origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-85,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",44.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",53.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",930000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",6430000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Province-wide spatial data management.\"],\n        AREA[\"Canada - Ontario.\"],\n        BBOX[41.67,-95.16,56.9,-74.35]],\n    ID[\"EPSG\",3161]]"]
                    }
                  ]
                },
                "n_empty": {
                  "type": "integer",
                  "attributes": {},
                  "value": [0]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [970121.15372838, 12486474.2531918]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [857050.92422645, 12647728.19049777]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [764502.63146004, 12897505.49960899]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [537797.09894598, 12499248.2902157]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [857050.92422645, 12647728.19049777]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [581985.6499293, 12830932.68160174]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [970121.15372838, 12486474.2531918]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [857050.92422645, 12647728.19049777]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [764502.63146004, 12897505.49960899]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [427866.72388268, 12063887.17557167]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [927855.89924228, 12598430.87102781]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [764502.63146004, 12897505.49960899]
                }
              ]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["siteID", "siteuse", "replsite", "lon_WGS84", "lat_WGS84", "stratum", "wgt", "ip", "caty", "aux", "file_name", "type", "path", "aru_type", "aru_id", "site_id", "date_time", "date", "tz", "t2sr", "t2ss", "doy", "psel_min", "psel_doy", "psel", "psel_scaled", "psel_std", "psel_normalized", "geometry"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1, 2]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["sf", "data.frame"]
            },
            "sf_column": {
              "type": "character",
              "attributes": {},
              "value": ["geometry"]
            },
            "agr": {
              "type": "integer",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["siteID", "siteuse", "replsite", "lon_WGS84", "lat_WGS84", "stratum", "wgt", "ip", "caty", "aux", "file_name", "type", "path", "aru_type", "aru_id", "site_id", "date_time", "date", "tz", "t2sr", "t2ss", "doy", "psel_min", "psel_doy", "psel", "psel_scaled", "psel_std", "psel_normalized"]
                },
                "levels": {
                  "type": "character",
                  "attributes": {},
                  "value": ["constant", "aggregate", "identity"]
                },
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["factor"]
                }
              },
              "value": ["NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["sample-13", "sample-14"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Over", "Over"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Next", "Next"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-90.38, -91.38]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [48.99, 45]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["None", "None"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [2.22708462, 2.22708462]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.44901752, 0.44901752]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["None", "None"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1, 1]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["P03_1_20200506T100000_ARU.wav", "P09_1_20200511T050000_ARU.wav"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["wav", "wav"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["a_BARLT10962_P03_1/P03_1_20200506T100000_ARU.wav", "j_S4A02222_P09_1/P09_1_20200511T050000_ARU.wav"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["BarLT", "SongMeter"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["BARLT10962", "S4A02222"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["P03_1", "P09_1"]
            },
            {
              "type": "double",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["POSIXct", "POSIXt"]
                },
                "tzone": {
                  "type": "character",
                  "attributes": {},
                  "value": ["UTC"]
                }
              },
              "value": [1588759200, 1589173200]
            },
            {
              "type": "double",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Date"]
                }
              },
              "value": [18388, 18393]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["America/Toronto", "America/Chicago"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [207.13333333, -41.88333333]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-685.88333333, 516.65]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [127, 132]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.84075194, -0.5147367]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.89091445, -0.82546313]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.17698923, 0.26179335]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.65524553, 0.96920542]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1, 1]
            },
            {
              "type": "double",
              "attributes": {},
              "value": ["NaN", "NaN"]
            },
            {
              "type": "list",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["sfc_POINT", "sfc"]
                },
                "precision": {
                  "type": "double",
                  "attributes": {},
                  "value": [0]
                },
                "bbox": {
                  "type": "double",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["xmin", "ymin", "xmax", "ymax"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["bbox"]
                    }
                  },
                  "value": [427866.72388268, 12063887.17557167, 537797.09894598, 12499248.2902157]
                },
                "crs": {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["input", "wkt"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["crs"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["EPSG:3161"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["PROJCRS[\"NAD83 / Ontario MNR Lambert\",\n    BASEGEOGCRS[\"NAD83\",\n        DATUM[\"North American Datum 1983\",\n            ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4269]],\n    CONVERSION[\"Ontario MNR Lambert\",\n        METHOD[\"Lambert Conic Conformal (2SP)\",\n            ID[\"EPSG\",9802]],\n        PARAMETER[\"Latitude of false origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-85,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",44.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",53.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",930000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",6430000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Province-wide spatial data management.\"],\n        AREA[\"Canada - Ontario.\"],\n        BBOX[41.67,-95.16,56.9,-74.35]],\n    ID[\"EPSG\",3161]]"]
                    }
                  ]
                },
                "n_empty": {
                  "type": "integer",
                  "attributes": {},
                  "value": [0]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [537797.09894598, 12499248.2902157]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [427866.72388268, 12063887.17557167]
                }
              ]
            }
          ]
        },
        {
          "type": "NULL"
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["call", "stratum_var", "stratum", "n_base", "seltype", "caty_var", "caty_n", "aux_var", "legacy", "mindis", "n_over", "n_near"]
            }
          },
          "value": [
            {
              "type": "language",
              "attributes": {},
              "value": ["spsurvey::grts(sframe = meta_weights_sf, n_base = n, stratum_var = sites_name, ", "    aux_var = nse_name(col_sel_weights), n_over = n_os, DesignID = \"sample\")"]
            },
            {
              "type": "NULL"
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["None"]
            },
            {
              "type": "double",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["None"]
                }
              },
              "value": [12]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["None"]
                }
              },
              "value": ["proportional"]
            },
            {
              "type": "NULL"
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["None"]
                }
              },
              "value": [
                {
                  "type": "NULL"
                }
              ]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["psel_std"]
            },
            {
              "type": "logical",
              "attributes": {},
              "value": [false]
            },
            {
              "type": "NULL"
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["None"]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {},
                  "value": [2]
                }
              ]
            },
            {
              "type": "NULL"
            }
          ]
        }
      ]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["sites_legacy", "sites_base", "sites_over", "sites_near", "design"]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["sp_design"]
        }
      },
      "value": [
        {
          "type": "NULL"
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["siteID", "siteuse", "replsite", "lon_WGS84", "lat_WGS84", "stratum", "wgt", "ip", "caty", "aux", "file_name", "type", "path", "aru_type", "aru_id", "site_id", "date_time", "date", "tz", "t2sr", "t2ss", "doy", "psel_min", "psel_doy", "psel", "psel_scaled", "psel_std", "psel_normalized", "geometry"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3, 4, 5, 6, 7, 8, 9]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["sf", "data.frame"]
            },
            "sf_column": {
              "type": "character",
              "attributes": {},
              "value": ["geometry"]
            },
            "agr": {
              "type": "integer",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["siteID", "siteuse", "replsite", "lon_WGS84", "lat_WGS84", "stratum", "wgt", "ip", "caty", "aux", "file_name", "type", "path", "aru_type", "aru_id", "site_id", "date_time", "date", "tz", "t2sr", "t2ss", "doy", "psel_min", "psel_doy", "psel", "psel_scaled", "psel_std", "psel_normalized"]
                },
                "levels": {
                  "type": "character",
                  "attributes": {},
                  "value": ["constant", "aggregate", "identity"]
                },
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["factor"]
                }
              },
              "value": ["NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["sample-01", "sample-02", "sample-03", "sample-04", "sample-05", "sample-06", "sample-07", "sample-08", "sample-09"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Base", "Base", "Base", "Base", "Base", "Base", "Base", "Base", "Base"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["None", "None", "None", "None", "None", "None", "None", "None", "None"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-85.03, -85.03, -87.45, -87.45, -87.45, -87.45, -87.45, -90.38, -90.38]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [50.01, 50.01, 52.68, 52.68, 52.68, 52.68, 52.68, 48.99, 48.99]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["P01_1", "P01_1", "P02_1", "P02_1", "P02_1", "P02_1", "P02_1", "P03_1", "P03_1"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1.5, 1.5, 1.2, 1.2, 1.2, 1.2, 1.2, 1.5, 1.5]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.66666667, 0.66666667, 0.83333333, 0.83333333, 0.83333333, 0.83333333, 0.83333333, 0.66666667, 0.66666667]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["None", "None", "None", "None", "None", "None", "None", "None", "None"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1, 1, 0.94411202, 1, 1, 0.94411202, 0.94411202, 1, 1]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["P01_1_20200503T052000_ARU.wav", "P01_1_20200503T052000_ARU.wav", "P02_1_20200504T052500_ARU.wav", "P02_1_20200505T073000_ARU.wav", "P02_1_20200505T073000_ARU.wav", "P02_1_20200504T052500_ARU.wav", "P02_1_20200504T052500_ARU.wav", "P03_1_20200506T100000_ARU.wav", "P03_1_20200506T100000_ARU.wav"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["a_BARLT10962_P01_1/P01_1_20200503T052000_ARU.wav", "o_BARLT10962_P01_1/P01_1_20200503T052000_ARU.wav", "a_S4A01234_P02_1/P02_1_20200504T052500_ARU.wav", "a_S4A01234_P02_1/P02_1_20200505T073000_ARU.wav", "j_S4A01234_P02_1/P02_1_20200505T073000_ARU.wav", "o_S4A01234_P02_1/P02_1_20200504T052500_ARU.wav", "j_S4A01234_P02_1/P02_1_20200504T052500_ARU.wav", "a_BARLT10962_P03_1/P03_1_20200506T100000_ARU.wav", "o_BARLT10962_P03_1/P03_1_20200506T100000_ARU.wav"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["BarLT", "BarLT", "SongMeter", "SongMeter", "SongMeter", "SongMeter", "SongMeter", "BarLT", "BarLT"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["BARLT10962", "BARLT10962", "S4A01234", "S4A01234", "S4A01234", "S4A01234", "S4A01234", "BARLT10962", "BARLT10962"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["P01_1", "P01_1", "P02_1", "P02_1", "P02_1", "P02_1", "P02_1", "P03_1", "P03_1"]
            },
            {
              "type": "double",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["POSIXct", "POSIXt"]
                },
                "tzone": {
                  "type": "character",
                  "attributes": {},
                  "value": ["UTC"]
                }
              },
              "value": [1588483200, 1588483200, 1588569900, 1588663800, 1588663800, 1588569900, 1588569900, 1588759200, 1588759200]
            },
            {
              "type": "double",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Date"]
                }
              },
              "value": [18385, 18385, 18386, 18387, 18387, 18386, 18386, 18388, 18388]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-53.21666667, -53.21666667, -47.25, 79.61666667, 79.61666667, -47.25, -47.25, 207.13333333, 207.13333333]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [498.41666667, 498.41666667, 483.41666667, 606.68333333, 606.68333333, 483.41666667, 483.41666667, -685.88333333, -685.88333333]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [124, 124, 125, 126, 126, 125, 125, 127, 127]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.53599721, -0.53599721, -0.52402648, -0.48126855, -0.48126855, -0.52402648, -0.52402648, -0.84075194, -0.84075194]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.93517202, -0.93517202, -0.92000393, -0.90525141, -0.90525141, -0.92000393, -0.92000393, -0.89091445, -0.89091445]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.22965681, 0.22965681, 0.23597476, 0.24994361, 0.24994361, 0.23597476, 0.23597476, 0.17698923, 0.17698923]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.85023024, 0.85023024, 0.87362044, 0.92533557, 0.92533557, 0.87362044, 0.87362044, 0.65524553, 0.65524553]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1, 1, 0.94411202, 1, 1, 0.94411202, 0.94411202, 1, 1]
            },
            {
              "type": "double",
              "attributes": {},
              "value": ["NaN", "NaN", 0.001, 1, 1, 0.001, 0.001, "NaN", "NaN"]
            },
            {
              "type": "list",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["sfc_POINT", "sfc"]
                },
                "precision": {
                  "type": "double",
                  "attributes": {},
                  "value": [0]
                },
                "bbox": {
                  "type": "double",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["xmin", "ymin", "xmax", "ymax"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["bbox"]
                    }
                  },
                  "value": [537797.09894598, 12499248.2902157, 927855.89924228, 12897505.49960899]
                },
                "crs": {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["input", "wkt"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["crs"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["EPSG:3161"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["PROJCRS[\"NAD83 / Ontario MNR Lambert\",\n    BASEGEOGCRS[\"NAD83\",\n        DATUM[\"North American Datum 1983\",\n            ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4269]],\n    CONVERSION[\"Ontario MNR Lambert\",\n        METHOD[\"Lambert Conic Conformal (2SP)\",\n            ID[\"EPSG\",9802]],\n        PARAMETER[\"Latitude of false origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-85,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",44.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",53.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",930000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",6430000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Province-wide spatial data management.\"],\n        AREA[\"Canada - Ontario.\"],\n        BBOX[41.67,-95.16,56.9,-74.35]],\n    ID[\"EPSG\",3161]]"]
                    }
                  ]
                },
                "n_empty": {
                  "type": "integer",
                  "attributes": {},
                  "value": [0]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [927855.89924228, 12598430.87102781]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [927855.89924228, 12598430.87102781]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [764502.63146004, 12897505.49960899]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [764502.63146004, 12897505.49960899]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [764502.63146004, 12897505.49960899]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [764502.63146004, 12897505.49960899]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [764502.63146004, 12897505.49960899]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [537797.09894598, 12499248.2902157]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [537797.09894598, 12499248.2902157]
                }
              ]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["siteID", "siteuse", "replsite", "lon_WGS84", "lat_WGS84", "stratum", "wgt", "ip", "caty", "aux", "file_name", "type", "path", "aru_type", "aru_id", "site_id", "date_time", "date", "tz", "t2sr", "t2ss", "doy", "psel_min", "psel_doy", "psel", "psel_scaled", "psel_std", "psel_normalized", "geometry"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["sf", "data.frame"]
            },
            "sf_column": {
              "type": "character",
              "attributes": {},
              "value": ["geometry"]
            },
            "agr": {
              "type": "integer",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["siteID", "siteuse", "replsite", "lon_WGS84", "lat_WGS84", "stratum", "wgt", "ip", "caty", "aux", "file_name", "type", "path", "aru_type", "aru_id", "site_id", "date_time", "date", "tz", "t2sr", "t2ss", "doy", "psel_min", "psel_doy", "psel", "psel_scaled", "psel_std", "psel_normalized"]
                },
                "levels": {
                  "type": "character",
                  "attributes": {},
                  "value": ["constant", "aggregate", "identity"]
                },
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["factor"]
                }
              },
              "value": ["NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["sample-10"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Over"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Next"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-87.45]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [52.68]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["P02_1"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1.2]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.83333333]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["None"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["P02_1_20200505T073000_ARU.wav"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["wav"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["o_S4A01234_P02_1/P02_1_20200505T073000_ARU.wav"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["SongMeter"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["S4A01234"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["P02_1"]
            },
            {
              "type": "double",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["POSIXct", "POSIXt"]
                },
                "tzone": {
                  "type": "character",
                  "attributes": {},
                  "value": ["UTC"]
                }
              },
              "value": [1588663800]
            },
            {
              "type": "double",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Date"]
                }
              },
              "value": [18387]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["America/Toronto"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [79.61666667]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [606.68333333]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [126]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.48126855]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.90525141]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.24994361]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.92533557]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1]
            },
            {
              "type": "list",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["sfc_POINT", "sfc"]
                },
                "precision": {
                  "type": "double",
                  "attributes": {},
                  "value": [0]
                },
                "bbox": {
                  "type": "double",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["xmin", "ymin", "xmax", "ymax"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["bbox"]
                    }
                  },
                  "value": [764502.63146004, 12897505.49960899, 764502.63146004, 12897505.49960899]
                },
                "crs": {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["input", "wkt"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["crs"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["EPSG:3161"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["PROJCRS[\"NAD83 / Ontario MNR Lambert\",\n    BASEGEOGCRS[\"NAD83\",\n        DATUM[\"North American Datum 1983\",\n            ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4269]],\n    CONVERSION[\"Ontario MNR Lambert\",\n        METHOD[\"Lambert Conic Conformal (2SP)\",\n            ID[\"EPSG\",9802]],\n        PARAMETER[\"Latitude of false origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-85,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",44.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",53.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",930000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",6430000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Province-wide spatial data management.\"],\n        AREA[\"Canada - Ontario.\"],\n        BBOX[41.67,-95.16,56.9,-74.35]],\n    ID[\"EPSG\",3161]]"]
                    }
                  ]
                },
                "n_empty": {
                  "type": "integer",
                  "attributes": {},
                  "value": [0]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [764502.63146004, 12897505.49960899]
                }
              ]
            }
          ]
        },
        {
          "type": "NULL"
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["call", "stratum_var", "stratum", "n_base", "seltype", "caty_var", "caty_n", "aux_var", "legacy", "mindis", "n_over", "n_near"]
            }
          },
          "value": [
            {
              "type": "language",
              "attributes": {},
              "value": ["spsurvey::grts(sframe = meta_weights_sf, n_base = n, stratum_var = sites_name, ", "    aux_var = nse_name(col_sel_weights), n_over = n_os, DesignID = \"sample\")"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["site_id"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["P01_1", "P02_1", "P03_1"]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["P01_1", "P02_1", "P03_1"]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {},
                  "value": [2]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [5]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [2]
                }
              ]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["P01_1", "P02_1", "P03_1"]
                }
              },
              "value": ["proportional", "proportional", "proportional"]
            },
            {
              "type": "NULL"
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["P01_1", "P02_1", "P03_1"]
                }
              },
              "value": [
                {
                  "type": "NULL"
                },
                {
                  "type": "NULL"
                },
                {
                  "type": "NULL"
                }
              ]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["psel_std"]
            },
            {
              "type": "logical",
              "attributes": {},
              "value": [false]
            },
            {
              "type": "NULL"
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["P01_1", "P02_1", "P03_1"]
                }
              },
              "value": [
                {
                  "type": "NULL"
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [1]
                },
                {
                  "type": "NULL"
                }
              ]
            },
            {
              "type": "NULL"
            }
          ]
        }
      ]
    }

