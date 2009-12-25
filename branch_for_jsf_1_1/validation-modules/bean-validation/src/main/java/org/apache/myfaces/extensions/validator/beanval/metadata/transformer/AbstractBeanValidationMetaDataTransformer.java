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
package org.apache.myfaces.extensions.validator.beanval.metadata.transformer;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.beanval.payload.ViolationSeverity;
import org.apache.myfaces.extensions.validator.beanval.payload.DisableClientSideValidation;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.validation.metadata.ConstraintDescriptor;
import javax.validation.Payload;
import javax.faces.application.FacesMessage;
import java.util.Map;
import java.util.Collections;
import java.lang.annotation.Annotation;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation({UsageCategory.REUSE})
public abstract class AbstractBeanValidationMetaDataTransformer<T extends Annotation> implements MetaDataTransformer
{
    @SuppressWarnings({"unchecked"})
    public Map<String, Object> convertMetaData(MetaDataEntry metaDataEntry)
    {
        ConstraintDescriptor<? extends T> constraintDescriptor = metaDataEntry.getValue(ConstraintDescriptor.class);

        if(isClientSideValidationEnabled(constraintDescriptor) && isBlockingConstraint(constraintDescriptor))
        {
            return transformMetaData((ConstraintDescriptor<T>)constraintDescriptor);
        }
        return Collections.emptyMap();
    }

    protected boolean isClientSideValidationEnabled(ConstraintDescriptor<? extends T> constraintDescriptor)
    {
        for(Class<? extends Payload> payload : constraintDescriptor.getPayload())
        {
            if(ExtValUtils.getValidationParameterClassFor(DisableClientSideValidation.class).isAssignableFrom(payload))
            {
                return false;
            }
        }
        return true;
    }

    protected boolean isBlockingConstraint(ConstraintDescriptor<?> constraintDescriptor)
    {
        FacesMessage testMessage = new FacesMessage();
        testMessage.setSeverity(ViolationSeverity.Error.VALUE);

        for (Class<? extends Payload> payload : constraintDescriptor.getPayload())
        {
            if (ExtValUtils.getValidationParameterClassFor(ViolationSeverity.Warn.class).isAssignableFrom(payload))
            {
                testMessage.setSeverity(ViolationSeverity.Warn.VALUE);
            }
            else if(ExtValUtils.getValidationParameterClassFor(ViolationSeverity.Info.class).isAssignableFrom(payload))
            {
                testMessage.setSeverity(ViolationSeverity.Info.VALUE);
            }
            else if(ExtValUtils.getValidationParameterClassFor(ViolationSeverity.Fatal.class).isAssignableFrom(payload))
            {
                testMessage.setSeverity(ViolationSeverity.Fatal.VALUE);
            }
        }
        return ExtValUtils.severityBlocksSubmitForComponentId(null, testMessage);
    }
    protected abstract Map<String, Object> transformMetaData(ConstraintDescriptor<T> constraintDescriptor);
}
