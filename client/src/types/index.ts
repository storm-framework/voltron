export interface Buffer {
  id:   number;
  hash: string;
  text: string;
}

export interface DivBuffer { 
  div: string;
  buf: Buffer;
}

export interface Student { 
  kind: "student";
  user: User;
  grpBuffer: Buffer;
}

export interface Instructor {
  kind: "instructor";
  user: User;
  allBuffers: Array<Buffer>;
} 

export interface None {
  kind: "none";
}

export type UserData = Student | Instructor | None;

export interface AuthInfo {
  emailAddress: string;
  password: string;
}

export interface User {
  firstName: string;
  lastName: string;
  group: string;
}