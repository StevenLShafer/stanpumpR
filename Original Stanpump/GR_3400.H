/************************************************************/
/* GR_3400.c					                           	*/
/*   Header for Graseby 3400 Syringe Pump Driver           	*/
/************************************************************/

/* constant enumerations */

enum	graseby_pump_command_code
			{
				START_INFUSION,
				STOP_INFUSION,
				CHANGE_RATE,
				GET_RATE,
				GET_TOTAL_DELIVERED,
				ZERO_TOTAL_DELIVERED,
				CHECK_MESSAGE,
				CHECK_RATE,
				NUMBER_GRASEBY_PUMP_COMMANDS
			};


enum	graseby_pump_status_code
			{
				STANDBY,
				INFUSING,
				ALARM,
				NUMBER_GRASEBY_PUMP_STATUSES,
				PRESTATUS
			};


/* special characters */
#define CR		0x0d	/* carriage return */
#define LF		0x0a	/* line feed */
#define ESC		0x1b	/* escape */

/* function prototypes */

void initialize_graseby_3400_pump ();
char *write_graseby_3400 ();
char *build_command_string ();
char *read_graseby_3400_vol_infused ();
void write_graseby_3400_pump_rate ();
void keep_graseby_pumping ();
void purge_graseby_3400();

/* global variables allocated in gr_3400.c */
extern int		graseby_status;
extern float	graseby_rate;
