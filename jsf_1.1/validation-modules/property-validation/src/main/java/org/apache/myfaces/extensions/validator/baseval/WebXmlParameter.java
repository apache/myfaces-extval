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
package org.apache.myfaces.extensions.validator.baseval;

import org.apache.myfaces.extensions.validator.util.WebXmlUtils;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

/**
 * centralized in order that these information arn't spread over the complete code base
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@ToDo(value = Priority.HIGH, description = "documentation")
@UsageInformation(UsageCategory.INTERNAL)
public interface WebXmlParameter
{
    static final String VALIDATION_MESSAGES_JPA = WebXmlUtils
            .getInitParameter("JPA_VALIDATION_ERROR_MESSAGES");
    static final String DEACTIVATE_JPA_BASED_VALIDATION = WebXmlUtils
            .getInitParameter("DEACTIVATE_JPA_BASED_VALIDATION");
}
