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

import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.BeanMetaDataTransformerAdapter;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import java.util.logging.Logger;

/**
 * Adapter to connect validation strategies with meta-data transformers,
 * if the validation strategy is defined as bean and e.g. spring creates a proxy.
 *
 * it isn't linked to jsr 303.
 *
 * it's just a helper for proxies - you just need it, if you define the validation strategy as bean and
 * e.g. spring creates a proxy for it.
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation({UsageCategory.REUSE})
public class BeanValidationStrategyAdapterImpl implements BeanValidationStrategyAdapter
{
    protected final Logger logger = Logger.getLogger(getClass().getName());

    private MetaDataTransformer metaDataTransformer;
    private ValidationStrategy validationStrategy;
    //optional fallback for internal caching
    private String validationStrategyClassName;

    public BeanValidationStrategyAdapterImpl()
    {
        logger.fine(getClass().getName() + " instantiated");
    }

    /**
     * {@inheritDoc}
     * Delegates to the validation to the wrapped ValidationStrategy.
     */
    public void validate(FacesContext facesContext,
                         UIComponent uiComponent,
                         MetaDataEntry metaDataEntry,
                         Object convertedObject)
    {
        this.validationStrategy.validate(facesContext, uiComponent, metaDataEntry, convertedObject);
    }

    public String getValidationStrategyClassName()
    {
        if(validationStrategy.getClass().getPackage() != null)
        {
            return validationStrategy.getClass().getName();
        }
        return validationStrategyClassName;
    }

    /**
     * {@inheritDoc}
     * When a metaDataTransformer is set, the class name is returned.  When the metaDataTransformer is an adapter
     * (BeanMetaDataTransformerAdapter) we ask the adapter for the name.
     */
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

    /**
     * Sets the wrapped ValidationStrategy of this adapter.
     * @param validationStrategy The ValidationStrategy to wrap by this adapter.
     */
    public void setValidationStrategy(ValidationStrategy validationStrategy)
    {
        this.validationStrategy = validationStrategy;
    }

    /**
     * The class name returned by the  {link getValidationStrategyClassName()} method when no ValidationStrategy is set.
     * @param validationStrategyClassName  The class name to return.
     */
    public void setValidationStrategyClassName(String validationStrategyClassName)
    {
        this.validationStrategyClassName = validationStrategyClassName;
    }
}
