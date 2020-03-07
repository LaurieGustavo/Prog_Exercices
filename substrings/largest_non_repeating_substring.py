#!/usr/bin/env python
# -*- coding: utf-8 -*-
import string


''' En faisant correspondre à chaque lettre sa place dans l'alphabet 
	(a -> 0, .. , z -> 25) on peut utiliser un tableau (accessible en O(1))
	qui pour chaque lettre indique sa dernière position connue
	(-1 si jamais vu ou par exemlpe 2 pour la lettre p dans alpaga)
'''

class Solution:

	# fait correspondre à une lettre sa place dans l'alphabet
	# Exempel : a -> 0 , z -> 25
	def alphanumber(self,c):
		return string.ascii_lowercase.index(c)

	def lengthOfLongestSubstring(self,s):
		s_size			= len(s)
		chars_pos 		= [-1 for i in range(26)]
		start_pos		= 0
		end_pos			= 1
		maxSize			= 1

		chars_pos[self.alphanumber(s[0])] = 0


		while(end_pos < s_size):
			# À quel position a-t-on déjà vu cette lettre ?
			# si < 0 		=> nulle part
			# si < start_pos=> on l'a vu avant mais osef car on considère
			#				   un subset où elle n'es pas présente  
			char_pos = chars_pos[self.alphanumber(s[end_pos])]
			
			# Si la lettre rencontré se trouve déjà dans le subset qui nous
			# intéresse alors il faut reréduire le subset en déplaçant le début
			if(char_pos >= start_pos):
				start_pos = char_pos+1
			
			# mettre à jour la position de la lettre qu'on lit
			chars_pos[self.alphanumber(s[end_pos])] = end_pos
			# calculer si la taille actuelle est la plus grande
			maxSize = max(end_pos+1-start_pos, maxSize)
			# dans tout les cas on peut déplacer la tête de lecture
			end_pos += 1

		return maxSize

print(Solution().lengthOfLongestSubstring('abrkaabcdefghijjxxx'))