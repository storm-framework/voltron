import { EnrollStudent, EnrollInfo, Enroll, Buffer } from "@/types";
import BufferService from "@/services/buffer";

class EnrollService {
  makeEnroll(className: string, infos: EnrollInfo[]): Enroll {
    const newBuffers: Buffer[] = TODO;
    const newEnrolls: EnrollStudent[] = TODO;
    return { newBuffers, newEnrolls };
  }
}

export default new EnrollService();
