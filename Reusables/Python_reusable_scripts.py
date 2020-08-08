Aug_Dec2017Data = {}
for sheet_name1 in Aug_Dec2017File.sheet_names:
    Aug_Dec2017Data[sheet_name1] = pd.read_excel(file_name, sheet_name=sheet_name1)