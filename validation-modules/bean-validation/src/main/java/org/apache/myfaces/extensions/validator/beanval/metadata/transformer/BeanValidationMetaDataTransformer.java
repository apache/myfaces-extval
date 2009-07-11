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

import org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;

import javax.validation.metadata.ConstraintDescriptor;
import java.util.Map;

/**
 * @author Gerhard Petracek
 * @since 1.x.3
 */
@ToDo(value = Priority.HIGH, description = "select final transformer")
public class BeanValidationMetaDataTransformer implements MetaDataTransformer
{
    public Map<String, Object> convertMetaData(MetaDataEntry metaData)
    {
        if(isSupportedConstraint(metaData.getValue(ConstraintDescriptor.class)))
        {
            //TODO
        }

        //TODO
        return null;
    }

    protected boolean isSupportedConstraint(ConstraintDescriptor constraintDescriptor)
    {
        if(!constraintDescriptor.getAnnotation().annotationType().getName().startsWith("javax.validation.constraints"))
        {
            return false;
        }

        String simpleName = constraintDescriptor.getAnnotation().annotationType().getSimpleName();

        return simpleName.equals("Size") || simpleName.equals("Pattern") || simpleName.equals("NotNull");
    }

}
