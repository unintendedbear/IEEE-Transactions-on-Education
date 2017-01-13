#!/usr/bin/python
# -*- coding: utf-8 -*-
"""preprocessing.py"""

import sys
import csv
import os

def main():

    if len(sys.argv) == 2:
        args = sys.argv
        theCSV = args[1]
        theOutputFile = os.path.splitext(args[1])[0]+"_processed.csv"
        with open (theCSV, newline='') as csvFile, open (theOutputFile, 'w', newline='') as csvOutputFile:
            csvReader = csv.DictReader(csvFile) # No salen en orden
            ##csvReader = csv.reader(csvFile, delimiter=',', quotechar='"') #Salen en orden

            variables = ['Timestamp', 'Girl', 'Class', 'Women_talks',
            'Talks_about_w', 'Talks_gendergap', 'Professors', 'Interest_eng',
            'Maths', 'Physics', 'Tech', 'Computer_science', 'Years_tech',
            'Years_computer_science', 'Interest_new_tech', 'Jobs',
            'Future_studies', 'Social_acceptance', 'Wealth', 'Creative_job',
            'Easy_job', 'Good_schedule', 'Job_impact', 'Engineering',
            'Eng_easy_access', 'Eng_easy_study', 'Eng_im_capable',
            'Eng_opportunities_good', 'Eng_job_fast', 'Eng_for_women_good',
            'Eng_for_men_good', 'Eng_for_geeks_good', 'Eng_not_easy_access',
            'Eng_not_easy_study', 'Eng_im_not_capable', 'Eng_opportunities_bad',
            'Eng_job_not_fast', 'Eng_for_women_bad', 'Eng_for_men_bad',
            'Eng_for_geeks_bad']

            csvWtriter = csv.DictWriter(csvOutputFile, fieldnames = variables)

            # FALTAN:
            #'Si has respondido que NO te los planteas, ¿por qué? (marca todas las que correspondan)': 'No soy capaz de estudiarlas',
            #'Si has respondido que SÍ te los planteas, ¿por qué? (marca todas las que correspondan)': '',

            for row in csvReader:
                csvWtriter.writerow({'Timestamp': row['Timestamp'],
                'Girl': row['¿Eres mujer?'],
                'Class': row['¿Qué proporción de chicos y chicas había en tu clase de tecnología/informática durante tu último curso?'],
                'Women_talks': row['Indica si algunas de las siguientes actividades han tenido lugar en tu colegio o instituto [Charlas dadas por mujeres científicas o ingenieras]'],
                'Talks_about_w': row['Indica si algunas de las siguientes actividades han tenido lugar en tu colegio o instituto [Charlas o discusiones sobre el trabajo de mujeres científicas o ingenieras]'],
                'Talks_gendergap': row['Indica si algunas de las siguientes actividades han tenido lugar en tu colegio o instituto [Charlas o discusiones sobre la diferencia entre el número de hombres y mujeres en la ciencia o la ingeniería]'],
                'Professors': row['¿Cómo han sido tus profesores/as en las asignaturas de tecnología/informática?'],
                'Interest_eng': row['¿Cuánto dirías que te interesa la ingeniería?'],
                'Maths': row['Indica tus nota media en las siguientes asignaturas (si alguna no la has tenido, no marques nada en esa asignatura) [Matemáticas]'],
                'Physics': row['Indica tus nota media en las siguientes asignaturas (si alguna no la has tenido, no marques nada en esa asignatura) [Física]'],
                'Tech': row['Indica tus nota media en las siguientes asignaturas (si alguna no la has tenido, no marques nada en esa asignatura) [Tecnología]'],
                'Computer_science': row['Indica tus nota media en las siguientes asignaturas (si alguna no la has tenido, no marques nada en esa asignatura) [Informática]'],
                'Years_tech': row['¿Cuántos años has cursado la asignatura de Tecnología?'],
                'Years_computer_science': row['¿Cuántos años has cursado la asignatura de Informática?'],
                'Interest_new_tech': row['¿Te interesan las nuevas tecnologías? (Móviles, televisores inteligentes, relojes inteligentes, etc.)'],
                'Jobs': row['¿En qué medida conoces las salidas profesionales que tienen las ingenierías?'],
                'Future_studies': row['Si tuvieses que decidir ahora qué harás después de este curso, ¿qué responderías?'],
                'Social_acceptance': row['¿Qué opinas de los informáticos? [Tienen prestigio social]'],
                'Wealth': row['¿Qué opinas de los informáticos? [Ganan mucho dinero]'],
                'Creative_job': row['¿Qué opinas de los informáticos? [Hacen trabajos variados y creativos]'],
                'Easy_job': row['¿Qué opinas de los informáticos? [Su trabajo es muy fácil]'],
                'Good_schedule': row['¿Qué opinas de los informáticos? [Tienen un buen horario laboral]'],
                'Job_impact': row['¿Qué opinas de los informáticos? [Su trabajo tiene un gran impacto en la sociedad]'],
                'Engineering': row['¿Te planteas los estudios superiores en una carrera tecnológica?'],
                'Eng_easy_access', 'Eng_easy_study', 'Eng_im_capable',
                'Eng_opportunities_good', 'Eng_job_fast', 'Eng_for_women_good',
                'Eng_for_men_good', 'Eng_for_geeks_good', 'Eng_not_easy_access',
                'Eng_not_easy_study', 'Eng_im_not_capable', 'Eng_opportunities_bad',
                'Eng_job_not_fast', 'Eng_for_women_bad', 'Eng_for_men_bad',
                'Eng_for_geeks_bad'})
    else:
        print ("Usa el método así: python preprocessing.py nombredelarchivo.csv")
        sys.exit()

if __name__=="__main__":
        main()
