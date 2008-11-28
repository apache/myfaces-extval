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
package org.apache.myfaces.extensions.validator.core.interceptor;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.faces.component.UIComponent;
import javax.faces.validator.ValidatorException;
import java.util.List;
import java.util.ArrayList;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultValidationExceptionInterceptor implements ValidationExceptionInterceptor
{
    protected final Log logger = LogFactory.getLog(getClass());
    private static List<ValidationExceptionInterceptor> validationExceptionInterceptors =
            new ArrayList<ValidationExceptionInterceptor>();

    public DefaultValidationExceptionInterceptor()
    {
        if(logger.isDebugEnabled())
        {
            logger.debug(getClass().getName() + " instantiated");
        }
    }

    public boolean afterThrowing(UIComponent uiComponent,
                                 MetaDataEntry metaDataEntry,
                                 Object convertedObject,
                                 ValidatorException validatorException)
    {
        boolean result = true;

        for(ValidationExceptionInterceptor validationExceptionInterceptor : validationExceptionInterceptors)
        {
            if(!validationExceptionInterceptor
                    .afterThrowing(uiComponent, metaDataEntry, convertedObject, validatorException))
            {
                result = false;
            }

            if(logger.isTraceEnabled())
            {
                logger.trace(
                        "afterThrowing of " + validationExceptionInterceptor.getClass().getName() + " called");
            }
        }

        return result;
    }

    @UsageInformation(UsageCategory.INTERNAL)
    public static void addValidationExceptionInterceptor(ValidationExceptionInterceptor validationExceptionInterceptor)
    {
        synchronized (DefaultValidationExceptionInterceptor.class)
        {
            validationExceptionInterceptors.add(validationExceptionInterceptor);
        }
    }
}
