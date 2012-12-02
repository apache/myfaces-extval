/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.extensions.validator.core.storage;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ViolationSeverityInterpreter;

/**
 * Interface for a {@link ViolationSeverityInterpreter} storage.
 * Used by add-ons to change the interpreter for the current request.
 *
 * @since x.x.3
 */
@UsageInformation(UsageCategory.API)
public interface ViolationSeverityInterpreterStorage
{
    /**
     * Puts the violationSeverityInterpreter into the storage.
     * @param violationSeverityInterpreter ViolationSeverityInterpreter to se tin the storage.
     */
    void setViolationSeverityInterpreter(ViolationSeverityInterpreter violationSeverityInterpreter);

    /**
     * Retrieves the violationSeverityInterpreter from the storage.
     * @return  the violationSeverityInterpreter from the storage.
     */
    ViolationSeverityInterpreter getViolationSeverityInterpreter();
}
