(princ (strcat "
					Author - Robert Stokłosa
					Available commands:
					CHAINAGE - automatically measures the chainage distance after indicating the road axis and the initial distance. 
                            The calculated value is saved in the attribute of the selected block.
") )

(defun C:CHAINAGE (/ roadAxis startDistKm startDistM blk blkName blkBasePnt polyPnt distToPnt meters kilometers attValue)
		(while
			(not
				(and (setq roadAxis (car (entsel "\nSelect the road axis: ")))
            (eq (cdr (assoc 0 (entget roadAxis)))"LWPOLYLINE")
      	        )
            )
 			(prompt "\nPlease select the polyline!")
 		) ;A loop to prevent selecting an object other than a polyline as the road axis.
		(setq	startDistKm (atoi (getstring "\nEnter the initial kilometer value of the road: "))
				startDistM (atoi (getstring "\nEnter the initial meter value of the road: "))
				kilometers 0
		) ;Saving the initial kilometer and meter values of the road.
		(while t 
			(setq 	blk (entsel "\nSelect the block to save the chainage: ")
					blkName (car blk)
					blkBasePnt (cadr blk)
					polyPnt (vlax-curve-getClosestPointTo roadAxis blkBasePnt) ;Projecting the chainage onto the road axis.
					distToPnt (atoi (rtos (vlax-curve-getDistAtPoint roadAxis polyPnt) 2 0))
					meters (+ startDistM distToPnt)
			) ;Measuring and calculating the chainage distance.
			(if (>= meters 1000)
					(setq	kilometers (/ meters 1000)
							meters (- meters (* 1000 kilometers))
					)
			)
			(setq attValue (strcat (itoa (+ startDistKm kilometers)) "+" (itoa meters)))
            ;Converting the calculated value into a chainage format.
			(foreach Att 	(vlax-invoke
                                (vlax-ename->vla-object blkName)
                                'GetAttributes
							)

				(if (= (strcase (vla-get-TagString Att)) "KM")
					    (progn (vla-put-textstring Att attValue) attValue)
				)
			) ;Saving the value in the block attribute.
		) ;A loop in which the chainage distance is measured and saved in the block attribute.
	(princ)
)