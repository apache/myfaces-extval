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
package org.apache.myfaces.extensions.validator.core.validation.strategy;

import org.apache.myfaces.extensions.validator.core.annotation.AnnotationEntry;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.BeanMetaDataTransformerAdapter;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

/**
 * it's just a helper for proxies - you just need it, if you define the validation strategy as bean and
 * e.g. spring creates a proxy for it.

 * adapter to connect validation strategies with meta-data transformers,
 * if the validation strategy is defined as bean and e.g. spring creates a proxy
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation({UsageCategory.REUSE})
public class BeanValidationStrategyAdapterImpl implements BeanValidationStrategyAdapter
{
    protected final Log logger = LogFactory.getLog(getClass());

    private MetaDataTransformer metaDataTransformer;
    private ValidationStrategy validationStrategy;
    //optional fallback for internal cashing
    private String validationStrategyClassName;

    public BeanValidationStrategyAdapterImpl()
    {
        if(logger.isDebugEnabled())
        {
            logger.debug(getClass().getName() + " instantiated");
        }
    }

    public void validate(FacesContext facesContext,
                         UIComponent uiComponent,
                         AnnotationEntry annotationEntry,
                         Object convertedObject)
    {
        this.validationStrategy.validate(facesContext, uiComponent, annotationEntry, convertedObject);
    }

    public String getValidationStrategyClassName()
    {
        if(validationStrategy.getClass().getPackage() != null)
        {
            return validationStrategy.getClass().getName();
        }
        return validationStrategyClassName;
    }

    public String getMetaDataTransformerClassName()
    {
        if(metaDataTransformer != null)
        {
            if(metaDataTransformer.getClass().getPackage() != null)
            {
                return metaDataTransformer.getClass().getName();
            }
            else
            {
                if(metaDataTransformer instanceof BeanMetaDataTransformerAdapter)
                {
                    return ((BeanMetaDataTransformerAdapter) metaDataTransformer ).getMetaDataTransformerClassName();
                }
            }
        }
        return null;
    }

    /*
     * generated
     */
    public MetaDataTransformer getMetaDataTransformer()
    {
        return metaDataTransformer;
    }

    public void setMetaDataTransformer(MetaDataTransformer metaDataTransformer)
    {
        this.metaDataTransformer = metaDataTransformer;
    }

    public ValidationStrategy getValidationStrategy()
    {
        return validationStrategy;
    }

    public void setValidationStrategy(ValidationStrategy validationStrategy)
    {
        this.validationStrategy = validationStrategy;
    }

    public void setValidationStrategyClassName(String validationStrategyClassName)
    {
        this.validationStrategyClassName = validationStrategyClassName;
    }
}
