with Ada.Containers.Synchronized_Queue_Interfaces;
--with Ada.Containers.Bounded_Synchronized_Queues;
--with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Streams;

package Home_Streams.Queues is

   package Stream_Element_Queue_Interfaces is new Ada.Containers.Synchronized_Queue_Interfaces (Ada.Streams.Stream_Element);

   --package Stream_Element_Queues is new Ada.Containers.Bounded_Synchronized_Queues (Stream_Element_Queue_Interfaces);


end Home_Streams.Queues;
