pro widg1

;; Set up the widget family
base = widget_base(/column)
but1 = widget_button(base, value='Hello')
but2 = widget_button(base, value='Goodbye')

;; Display the widget family
widget_control, base, /realize

;; Pass the widget to the controller, starts the event loop
xmanager, 'widg1', base, event='widg1_event'

END

pro widg1_event, event

widget_control, event.id, get_value=test
case test of
  'Hello' : begin 
                  print, 'Hello there!'
                  i = indgen(10)
                  a = sin(i) 
                  plot,i,a,back=255,color=0
            end 
  'Goodbye' : widget_control, event.top, /destroy
ENDCASE

END
