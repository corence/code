
nodes = [
            (Receiver 0 1),
            (Broadcaster 1),
            (Receiver 2 2),
            (Broadcaster 3),
            (Receiver 4 1),
            (Source 5 2),
            (Sender 6 0 1),
            (Sender 7 0 1)
        ]

channels = [
               [1, 0],
               [1, 2],
               [3, 2],
               [3, 4],
               [5, 0],
               [5, 1],
               [5, 2],
               [5, 3],
               [5, 4],
               [5, 6],
               [5, 7],
               [6, 7],
               [7, 6]
           ]

links = []

solve nodes [] links = links
solve nodes ([source, dest]:channels) links
| 
    
