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
package org.apache.myfaces.extensions.validator.trinidad;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.WebXmlUtils;

/**
 * centralized in order that these information aren't spread over the complete code base
 *
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
interface WebXmlParameter
{
    /*
     * deactivate
     */
    static final String DEACTIVATE_CLIENT_SIDE_TRINIDAD_VALIDATION = WebXmlUtils
        .getInitParameter("DEACTIVATE_CLIENT_SIDE_TRINIDAD_VALIDATION");

    static final String DEACTIVATE_TRINIDAD_CORE_OUTPUT_LABEL_INITIALIZATION = WebXmlUtils
        .getInitParameter("DEACTIVATE_TRINIDAD_CORE_OUTPUT_LABEL_INITIALIZATION");

    static final String DEACTIVATE_TRINIDAD_VALIDATION_EXCEPTION_INTERCEPTOR = WebXmlUtils
        .getInitParameter("DEACTIVATE_TRINIDAD_VALIDATION_EXCEPTION_INTERCEPTOR");
}