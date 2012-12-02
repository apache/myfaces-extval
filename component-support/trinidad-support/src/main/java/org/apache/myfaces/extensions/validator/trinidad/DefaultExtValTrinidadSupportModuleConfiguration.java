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

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

/**
 * @since r4
 */
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultExtValTrinidadSupportModuleConfiguration extends ExtValTrinidadSupportModuleConfiguration
{
    /*
     * web.xml config
     */

    public boolean deactivateClientSideValidation()
    {
        return "true".equalsIgnoreCase(WebXmlParameter.DEACTIVATE_CLIENT_SIDE_TRINIDAD_VALIDATION);
    }

    public boolean deactivateCoreOutputLabelInitialization()
    {
        return "true".equalsIgnoreCase(WebXmlParameter.DEACTIVATE_TRINIDAD_CORE_OUTPUT_LABEL_INITIALIZATION);
    }

    public boolean deactivateValidationExceptionInterceptor()
    {
        return "true".equalsIgnoreCase(WebXmlParameter.DEACTIVATE_TRINIDAD_VALIDATION_EXCEPTION_INTERCEPTOR);
    }
}
