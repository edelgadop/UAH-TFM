import json
import codecs
import csv

# Lista con ficheros json de entrada. Todos deben tener la misma estructura
ficheros_entrada = ['on_street_parkings_ids_1_1000_modif.json',
                   'on_street_parkings_ids_1001_1598.json']
# Fichero de salida
fichero_salida = 'on_street_parkings_csv.csv'

# Une las filas de los ficheros de entrada en una unica lista
# Precacuion con la codificacion. Cambiar si es distinta en
# cada iteración.
filas = []
codificacion = 'utf-8-sig'
for file in ficheros_entrada:
    data = json.load(codecs.open(file, 'r', codificacion))
    
    '''Flatten de geometry. No esta en los dos ficheros
    for d in data:
        print(i)
        a = d['geometry'].get('x')
        d['attributes']['geom_x'] = d['geometry'].get('x')
        d['attributes']['geom_y'] = d['geometry'].get('y')
        i += 1'''
    
    filas += [element['attributes'] for element in data] # Concatenación de listas
    campos = data[0]['attributes'].keys()

# Escribe las filas en el fichero especificado
with open(fichero_salida, 'w', newline='') as f:
    writer = csv.DictWriter(f, campos)
    writer.writeheader()
    writer.writerows(filas)
