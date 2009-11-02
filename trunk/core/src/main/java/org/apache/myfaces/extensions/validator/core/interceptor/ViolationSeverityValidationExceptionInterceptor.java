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

import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ViolationSeverity;
import org.apache.myfaces.extensions.validator.core.InvocationOrder;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.faces.component.UIComponent;
import javax.faces.validator.ValidatorException;
import javax.faces.application.FacesMessage;
import java.lang.annotation.Annotation;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@InvocationOrder(90)
@UsageInformation(UsageCategory.INTERNAL)
@ToDo(value = Priority.HIGH, description = "check compatibility with bv-integration")
public class ViolationSeverityValidationExceptionInterceptor implements ValidationExceptionInterceptor
{
    protected final Log logger = LogFactory.getLog(getClass());

    public boolean afterThrowing(UIComponent uiComponent,
                                 MetaDataEntry metaDataEntry,
                                 Object convertedObject,
                                 ValidatorException validatorException,
                                 ValidationStrategy validatorExceptionSource)
    {
        if(isExtValMetaData(metaDataEntry))
        {
            tryToPlaceSeverity(validatorException, metaDataEntry.getValue(Annotation.class));
        }
        return true;
    }

    private boolean isExtValMetaData(MetaDataEntry metaDataEntry)
    {
        return metaDataEntry.getValue() instanceof Annotation;
    }

    private void tryToPlaceSeverity(ValidatorException validatorException, Annotation annotation)
    {
        for(FacesMessage.Severity severity : ExtValUtils.getValidationParameterExtractor()
                .extract(annotation, ViolationSeverity.class, FacesMessage.Severity.class))
        {
            validatorException.getFacesMessage().setSeverity(severity);
        }
    }
}
