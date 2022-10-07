 # import ORKG comparisons and export them as individual csv files

def p2csv_comparisons(comp_ids):
    import pandas as pd
    from orkg import ORKG

    orkg = ORKG(host='https://orkg.org/orkg', simcomp_host='https://orkg.org/orkg/simcomp')

    for i in range(len(comp_ids)):
        df = orkg.contributions.compare_dataframe(comparison_id= comp_ids[i])
        df = df.T
        df.comp_id = comp_ids[i]

        #correction of all comparison column names
        df=df.rename(columns={'stand of hypothesis':'support for hypothesis'})

        # prepocessing
        df['study'] = df.index

        # reset row indexing as numbers
        df = df.reset_index()
        
        # Split study name into publication and contribution, and add publication as new column
        df['publication'] = df['study'].str.split('/', expand = True)[0]

        # extract name of hypothesis
        hyp = pd.unique(df.hypothesis)[0]

        # export table as .csv
        print('exporting comparison for ' + hyp + ' hypothesis')
        df.to_csv("data/csv/comparison_" + comp_ids[i]+ "_" + hyp + ".csv",
        index = False, header= True)

comp_refs = ['R53407','R58002','R57501','R57101','R56110','R56945','R55219','R54867','R54244','R52143']
p2csv_comparisons(comp_refs)